{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Git operations not necessarily specific to mog.
--
-- The functions in this module are monomorphic over the "gitlib" backend;
-- specifically they use the "gitlib-libgit2" backend.
module Mog.Git.Libgit where

import Control.Concurrent.Async (withAsync)
import Control.Exception (Exception, throwIO, bracket)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (bimap)
import Data.List (intersperse)
import Data.Text (Text)
import System.FilePath ((</>))
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

import Mog.MergeDriver.Main (say)
import Mog.MergeDriver.Handler (withUDLSock, mergeDriverHandlerLoop)
import Mog.Git.Mutex (PidSymlinkError, withPidSymlink)

-- * Initialization

data StoreError
    = PathAlreadyExists FilePath
    | MutexError PidSymlinkError
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
    | Clone{local::FilePath, origin::String}
    | Open{local::FilePath}

-- | Initialize, clone, or open a store.
--
-- Might throw 'StoreError' on initialization.
withStore :: Config -> ReaderT GLG2.LgRepo IO a -> IO a
withStore config action = do
    repoOpts <- liftIO $ case config of
        Init{}  -> startInitClone
        Clone{} -> startInitClone
        Open{}  -> startOpen
    liftIO
        -- XXX there's no way to know whether all the downstream actions
        -- succeeded, and if they didn't then this repo will be left on disk
        . withRepo    repoOpts $ \repo ->
          withMutex   (pidfilePath $ gitdir repo)
        . withUDLSock (socketPath $ gitdir repo) $ \sock ->
          -- XXX mergeDriverHandlerLoop must be instantiated with a callback
          -- able to look up merge functions and merge data according to the UI
          -- types, but so far the user's data is not here?
          withAsync (mergeDriverHandlerLoop sock) $ \_async ->
          -- TODO: git config
          -- TODO: git attributes
          Git.runRepository GLG2.lgFactory repo
        $ action
  where
    assertLocalPathDoesntExist = do
        pathExists <- Dir.doesPathExist $ local config
        if pathExists
            then throwIO . PathAlreadyExists $ local config
            else return ()
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


--
-- @@
-- withRepo =
--     Ini Clo Ope
--     [x] [x] [ ] require the path "local" does not already exist
--     [T] [T] [?] set RepoOptions.IsBare [for open use: not(exists(local/.git)); emit a warning if IsBare is False]
--     [T] [T] [F] set RepoOptions.AutoCreate
--     XXXXXXXXXXX call withRepository'
--                     [DO NOT USE withRepository (takes no options) NOR withNewRepository NOR withNewRepository' (these delete the directory if it is already present)]
--     XXXXXXXXXXX bracket: (mutual exclusion)
--                     [acquire: symlink repo/mog.pid↦PID, read repo/mog.pid, and assert correct; use unix:System.Posix.getProcessID]
--                     [release: delete repo/mog.pid]
--     XXXXXXXXXXX bracket: (merge driver handler)
--                     [acquire: open listening socket]
--                     [release: close socket]
--     XXXXXXXXXXX withAsync (merge driver handler thread)
--     XXXXXXXXXXX bracket: (gitconfig)
--                     [acquire: install merge-driver cli]
--                     [release: uninstall]
--     XXXXXXXXXXX bracket: (gitattributes)
--                     [acquire: (gitattributes) associate pk/val extensions with binary/custom merge drivers (resp)]
--                     [release: remove associations]
--     XXXXXXXXXXX user-action
-- @@

-- | Will create if not existing, could be bare or worktree.
--
-- XXX: not used
withBackendRepo :: GLG2.MonadLg m => FilePath -> ReaderT GLG2.LgRepo m a -> m a
withBackendRepo path action = do
    notBare <- liftIO . Dir.doesPathExist $ path </> ".git"
    Git.withRepository'
        GLG2.lgFactory
        Git.RepositoryOptions
            { Git.repoPath = path
            , Git.repoIsBare = not notBare
            , Git.repoAutoCreate = True
            , Git.repoWorkingDir = Nothing
            }
        action

withRepo :: Git.RepositoryOptions -> (GLG2.LgRepo -> IO a) -> IO a
withRepo options =
    bracket
        (Git.openRepository GLG2.lgFactory options)
        (\repo -> Git.runRepository GLG2.lgFactory repo Git.closeRepository)

withMutex :: FilePath -> IO a -> IO a
withMutex path
    =   either (throwIO . MutexError) return
    <=< withPidSymlink path


-- * Paths

newtype Gitdir = Gitdir FilePath

-- | Obtain gitdir in a git monad.
getGitdir :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m Gitdir
getGitdir = gitdir <$> Git.getRepository

-- | Extract gitdir from a git repo.
gitdir :: GLG2.LgRepo -> Gitdir
gitdir repo =
    let opts = GLG2.repoOptions repo in
    Gitdir $
        if Git.repoIsBare opts
        then Git.repoPath opts
        else Git.repoPath opts </> ".git"

-- | Where's the socket?
socketPath :: Gitdir -> FilePath
socketPath (Gitdir repoRoot) = repoRoot </> "mog.sock"

-- | Where's the pidfile?
pidfilePath :: Gitdir -> FilePath
pidfilePath (Gitdir repoRoot) = repoRoot </> "mog.pid"




-- * Git config

newtype GitConfig = GitConfig GCP.GitConfig deriving Eq

instance Show GitConfig where
    show = Text.unpack . showGitConfig

gitConfigPath :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m FilePath
gitConfigPath = do
    Gitdir repoRoot <- getGitdir
    return $ repoRoot </> "config"

readGitConfig :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m Text
readGitConfig = liftIO . TIO.readFile =<< gitConfigPath

writeGitConfig :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => GitConfig -> m (Either String ())
writeGitConfig config
    | safeGitConfig config = do
        path <- gitConfigPath
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
    updatedConfig (GitConfig sections) = GitConfig $ case mdM of
        Nothing -> filter (not . match) sections -- XXX filter is "keep"
        Just md -> sections ++ [mergeDriverSection mdID md]
