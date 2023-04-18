{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Git operations not necessarily specific to mog.
--
-- The functions in this module are monomorphic over the "gitlib" backend;
-- specifically they use the "gitlib-libgit2" backend.
module Mog.Git.Libgit where

import Control.Concurrent.Async (withAsync)
import Control.Exception (Exception, throwIO, bracket, bracket_)
import Control.Exception (catch)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (bimap)
import Data.List (intersperse)
import Data.Tagged (Tagged(..), untag)
import Data.Text (Text)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.IO.Error (isDoesNotExistError)
import Data.Proxy (Proxy(..))
import qualified Data.HashMap.Strict as HashMap (toList, fromList)
import qualified Data.Text as Text hiding (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified System.Directory as Dir
import qualified System.Process as Proc

import qualified Git
import qualified Git.Libgit2 as GLG2
import qualified Text.GitConfig.Parser as GCP

import Mog.Output (pkTag, valTag)
import Mog.MergeDriver.Main (say)
import Mog.MergeDriver.Handler (withUnixDomainListeningSocket, mergeDriverHandlerLoop)
import Mog.MergeDriver.Merge (FindAndMerge(..))
import Mog.Git.Mutex (PidSymlinkError, withPidSymlink)

-- * Initialization

data StoreError
    = PathAlreadyExists FilePath
    | MutexError PidSymlinkError
    | FetchFailed
    | FetchSucceededButNoFetchHead
    | MergeDriverConfigError String
    deriving Show
instance Exception StoreError

-- | How to start up the store?
--
--  * 'Init' I don't have a repo on disk yet, and I'm starting a new one.
--
--  * 'Clone' I don't have a repo on disk yet, and I'm starting from somebody else's repo.
--
--  * 'Open' I do have a repo on disk already.
data Config
    = Init{local::FilePath}
    | Clone{local::FilePath, origin::Text}
    | Open{local::FilePath}

-- | Initialize, clone, or open a store.
--
-- Might throw 'StoreError' on initialization.
withStore :: FindAndMerge s => Config -> Proxy s -> ReaderT GLG2.LgRepo IO a -> IO a
withStore config schema action = do
    repoOpts <- liftIO $ case config of
        Init{}  -> startInitClone
        Clone{} -> startInitClone
        Open{}  -> startOpen
    -- TODO: audit each of these bracket-pattern functions to make sure we
    -- throw errors on setup, but only print warnings on teardown; there's no
    -- point in ruining a users' day on teardown if their action was able to
    -- produce its result successfully
    liftIO
        -- XXX there's no way to know whether all the downstream actions
        -- succeeded, and if they didn't then this repo will be left on disk
        . withRepo repoOpts
        $ \repo ->
          withMutex (pidfilePath $ repoGitdir repo)
        . withUnixDomainListeningSocket (socketPath $ repoGitdir repo)
        $ \sock ->
          -- XXX mergeDriverHandlerLoop must be instantiated with a callback
          -- able to look up merge functions and merge data according to the UI
          -- types, but so far the user's data is not here?
          withAsync (mergeDriverHandlerLoop sock schema)
        $ \_async ->
          withMogMergeDriverConfig repo
        . withMogFileExtAttributes repo
        . Git.runRepository GLG2.lgFactory repo
        $ do
        case config of
            Init{} -> return ()
            Clone{origin} -> finishClone origin
            Open{} -> return ()
        action
  where
    -- Before the repo is opened, assert it doesn't exist.
    assertLocalPathDoesntExist = do
        pathExists <- Dir.doesPathExist $ local config
        if pathExists
            then throwIO . PathAlreadyExists $ local config
            else return ()
    -- Before the repo is opened, check if it's a bare repo.
    checkRepoIsBare = do
        gitdirExists <- Dir.doesPathExist $ local config </> ".git"
        if gitdirExists
            then say $ "WARNING: won't update working-tree; prefer to use a bare-repo"
            else return ()
        return $ not gitdirExists
    -- Require the "local" path doesn't exist. Auto-create a new bare-repo.
    startInitClone = do
        assertLocalPathDoesntExist
        return Git.RepositoryOptions
            { Git.repoPath       = local config
            , Git.repoIsBare     = True
            , Git.repoAutoCreate = True
            , Git.repoWorkingDir = Nothing
            }
    finishClone origin = do
        coid <- fetchHead origin
        Git.updateReference "HEAD" $ Git.RefObj (untag coid)
    -- Assume the "local" path exists, do not auto-create. Check whether any
    -- existing repo is bare.
    startOpen = do
        repoIsBare <- checkRepoIsBare
        return Git.RepositoryOptions
            { Git.repoPath       = local config
            , Git.repoIsBare     = repoIsBare
            , Git.repoAutoCreate = False
            , Git.repoWorkingDir = Nothing
            }

-- | Wrapper that gives the repo in IO instead of running a reader transformer.
withRepo :: Git.RepositoryOptions -> (GLG2.LgRepo -> IO a) -> IO a
withRepo options =
    bracket
        (Git.openRepository GLG2.lgFactory options)
        (\repo -> Git.runRepository GLG2.lgFactory repo Git.closeRepository)

-- | Wrapper for 'withPidSymlink' that throws any errors it returns.
withMutex :: FilePath -> IO a -> IO a
withMutex path
    =   either (throwIO . MutexError) return
    <=< withPidSymlink path

-- | Bracket to (un)install the mog merge driver in git-config. Throw any
-- errors encountered while doing so.
withMogMergeDriverConfig :: GLG2.LgRepo -> IO a -> IO a
withMogMergeDriverConfig repo action = do
    execPath <- liftIO . Dir.makeAbsolute =<< getExecutablePath
    sockPath <- liftIO . Dir.makeAbsolute . socketPath $ repoGitdir repo
    let md = mkMD execPath sockPath
        go =   either (throwIO . MergeDriverConfigError) return
           <=< Git.runRepository GLG2.lgFactory repo
    bracket_
        (go $ setMergeDriver mergeDriverId (Just md))
        (go $ setMergeDriver mergeDriverId Nothing)
        action
  where
    mkMD exec sock = MergeDriver
        { mdName = "The Legend of Melda: A Link Across Versions"
        , mdRecursiveMD = Nothing
        , mdCommand = Text.unwords
            [ Text.pack exec
            , "--socketPath", Text.pack sock
            , "--mergeAncestor", "%0"
            , "--currentVersion", "%A"
            , "--otherBranchVersion", "%B"
            , "--conflictMarkerSize", "%L"
            , "--mergedResultPathname", "%P"
            ]
        }

-- | Bracket to (dis)associate file extensions with the mog merge driver.
--
-- FIXME: Currently this function replaces any existing attributes file during
-- the action.
--
-- We control the merging of keys and values with suffixes:
--
-- <gitdir>/info/attributes := ```
-- *.pk merge=binary
-- *.val merge=custom-driver
-- *.blah merge=blah-driver # hypothetically we might want to give users the option to use existing merge drivers
-- ```
--
-- As an alternative to placing these configs at the top level, we could place
-- them at the relation level by outputting the names of tuple columns:
-- <tree>/<datatype>/<relation>/.gitattributes := ```
-- */{pkfilecolumns} merge=binary
-- */{valuecolumns} merge=custom-driver
-- ```
--
-- What if a row contains a foreign key? Foreign keys are always referring to
-- primary keys which don't merge (merge=binary). The foreign key will be
-- represented as a subtree containing primary key columns.
withMogFileExtAttributes :: GLG2.LgRepo -> IO a -> IO a
withMogFileExtAttributes repo =
    bracket_
        (backup >> associate)
        restore
  where
    attrPath = gitAttributesPath $ repoGitdir repo
    attrBak = attrPath <.> "bak"
    backup = Dir.renamePath attrPath attrBak
        `catch` \e -> if isDoesNotExistError e then return () else throwIO e
    restore = Dir.renamePath attrBak attrPath
        `catch` \e -> if isDoesNotExistError e then return () else throwIO e
    associate = TIO.writeFile attrPath $ Text.unlines
        [ "*." <> pkTag  <> " merge=binary" -- NOTE: a conflict among primary keys indicates a hash collision for their paths
        , "*." <> valTag <> " merge=" <> mergeDriverId -- NOTE: these conflicts must be resolved by domain-specific three way merge
        ]

-- | Fetch HEAD from a location. The location is passed directly to @git
-- fetch@, and can be a file-path, url, or a remote.
--
-- `git fetch $URL HEAD` fetches HEAD from $URL and stores it in FETCH_HEAD.
fetchHead :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => Text -> m GLG2.CommitOid
fetchHead location = do
    Gitdir repoRoot <- getGitdir
    -- TODO: handle subprocess errors better
    -- TODO: find general patterns of how we call subprocesses
    status <- liftIO
        . Proc.withCreateProcess
            (Proc.proc "git" ["fetch", Text.unpack location, "HEAD"])
            { Proc.cwd = Just repoRoot
            , Proc.delegate_ctlc = True }
        $ \Nothing Nothing Nothing -> Proc.waitForProcess
    case status of
        ExitSuccess -> do
            moid <- Git.resolveReference "FETCH_HEAD"
            case moid of
                Nothing -> liftIO $ throwIO FetchSucceededButNoFetchHead
                Just oid -> return $ Tagged oid
            -- FIXME: to reduce the chance of a race, instead of calling
            -- resolveReference here: match the given location to the location
            -- in repoRoot</>FETCH_HEAD and use the OID from there too.
        ExitFailure{} -> do
            liftIO $ throwIO FetchFailed

-- * Paths

newtype Gitdir = Gitdir FilePath

-- | Obtain gitdir in a git monad.
getGitdir :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m Gitdir
getGitdir = repoGitdir <$> Git.getRepository

-- | Extract gitdir from a git repo based on whether the repo is bare.
repoGitdir :: GLG2.LgRepo -> Gitdir
repoGitdir repo =
    let opts = GLG2.repoOptions repo in
    Gitdir $
        if Git.repoIsBare opts
        then Git.repoPath opts
        else Git.repoPath opts </> ".git"

gitConfigPath :: Gitdir -> FilePath
gitConfigPath (Gitdir repoRoot) = repoRoot </> "config"

gitAttributesPath :: Gitdir -> FilePath
gitAttributesPath (Gitdir repoRoot) = repoRoot </> "info" </> "attributes"

-- | Where's the mog socket?
socketPath :: Gitdir -> FilePath
socketPath (Gitdir repoRoot) = repoRoot </> "mog.sock"

-- | Where's the mog pidfile?
pidfilePath :: Gitdir -> FilePath
pidfilePath (Gitdir repoRoot) = repoRoot </> "mog.pid"

-- | What's the git-config identifier for the mog merge driver?
mergeDriverId :: Text
mergeDriverId = "mog-md"




-- * Git config

newtype GitConfig = GitConfig GCP.GitConfig deriving Eq

instance Show GitConfig where
    show = Text.unpack . showGitConfig

readGitConfig :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m Text
readGitConfig = liftIO . TIO.readFile . gitConfigPath =<< getGitdir

writeGitConfig :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => GitConfig -> m (Either String ())
writeGitConfig config
    | safeGitConfig config = do
        path <- gitConfigPath <$> getGitdir
        liftIO . TIO.writeFile path $ showGitConfig config
        return $ pure ()
    | otherwise = return $ Left "not safe to write new git config"

showGitConfig :: GitConfig -> Text
showGitConfig (GitConfig sections)
    = TL.toStrict . TB.toLazyText . mconcat $ map showSection sections
  where
    fromStrict = TB.fromLazyText . TL.fromStrict :: Text -> TB.Builder
    fromString = TB.fromLazyText . TL.pack :: String -> TB.Builder
    showSection (GCP.Section [section, subsec] kvs) = "[" <> fromStrict section <> " " <> showSubsec subsec <> "]\n" <> showKVs kvs <> "\n"
    showSection (GCP.Section [section]         kvs) = "[" <> fromStrict section <>                             "]\n" <> showKVs kvs <> "\n"
    showSection (GCP.Section _section_subsec  _kvs) = error "only sections and subsections are supported: https://git-scm.com/docs/git-config#_syntax"
    showSubsec = fromString . show :: Text -> TB.Builder -- Quoted string
    showKVs
        = mconcat . intersperse (TB.singleton '\n')
        . fmap (uncurry $ \k v -> "    " <> k <> " = " <> v)
        . fmap (bimap fromStrict fromStrict)
        . HashMap.toList

parseGitConfig :: Text -> Either String GitConfig
parseGitConfig = bimap show GitConfig . GCP.parseConfig

-- | Helper for validating our git config serializer against the parser.
--
--  * Did parsing produce a correct result? Assume yes.
--
--  * Can we safely write the parsed config back to disk? Yes if it parses to
--  the same value again after serialization.
--
--  * Can we safely modify the parsed config and write it back to disk? Yes if
--  it parses to the same value again after serialization.
safeGitConfig :: GitConfig -> Bool
safeGitConfig gc =
    case parseGitConfig $ showGitConfig gc of
        Left{} -> False
        Right gc' -> gc == gc'




-- * Merge driver

-- | <https://git-scm.com/docs/gitattributes#_built_in_merge_drivers>
data MergeDriver = MergeDriver
    { mdName :: Text
    , mdCommand :: Text
    , mdRecursiveMD :: Maybe Text
    }

-- | Merge driver identifier should be ascii letters and numbers without spaces or
-- punctuation.
mergeDriverSection :: Text -> MergeDriver -> GCP.Section
mergeDriverSection mdID MergeDriver{mdName,mdCommand,mdRecursiveMD} =
    GCP.Section ["merge", mdID]
        . HashMap.fromList
        $ [ ("name", mdName)
          , ("driver", mdCommand)
          ]
        ++ (maybe [] (\n -> [("recursive", n)]) mdRecursiveMD)

-- | Update or remove a merge driver in the git config.
setMergeDriver :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => Text -> Maybe MergeDriver -> m (Either String ())
setMergeDriver mdID mdM = do
    parsed <- parseGitConfig <$> readGitConfig
    case parsed of
        Left err -> return $ Left err
        Right config
            | safeGitConfig config -> writeGitConfig $ updatedConfig config
            | otherwise -> return $ Left "not safe to overwrite existing git config"
  where
    match (GCP.Section ["merge", otherID] _) = mdID == otherID
    match _ = False
    updatedConfig (GitConfig sections) = GitConfig $ case mdM of
        Nothing -> filter (not . match) sections -- XXX filter is "keep"
        Just md -> sections ++ [mergeDriverSection mdID md]
