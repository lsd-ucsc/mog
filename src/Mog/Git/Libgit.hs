{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Git operations not necessarily specific to mog.
--
-- The functions in this module are monomorphic over the "gitlib" backend;
-- specifically they use the "gitlib-libgit2" backend.
module Mog.Git.Libgit where

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

import qualified Git
import qualified Git.Libgit2 as GLG2
import qualified Text.GitConfig.Parser as GCP

withRepo :: GLG2.MonadLg m => FilePath -> ReaderT GLG2.LgRepo m a -> m a
withRepo path action = do
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



-- * Paths

newtype Gitdir = Gitdir FilePath

getGitdir :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m Gitdir
getGitdir = do
    opts <- GLG2.repoOptions <$> Git.getRepository
    return . Gitdir $
        if Git.repoIsBare opts
        then Git.repoPath opts
        else Git.repoPath opts </> ".git"




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
