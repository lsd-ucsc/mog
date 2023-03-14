{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module for interacting with a git backend to output/input a
-- 'Output.Database' value to/from disk
--
-- The functions in this module are monomorphic over the "gitlib" backend;
-- specifically they use the "gitlib-libgit2" backend.
module Mog.GitLibgit where

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Text (Text)
import System.FilePath ((</>))
import Data.HashMap.Strict as HashMap (toList)
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

gitConfigPath :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m FilePath
gitConfigPath = do
    opts <- GLG2.repoOptions <$> Git.getRepository
    return $
        if Git.repoIsBare opts
        then Git.repoPath opts </> "config"
        else Git.repoPath opts </> ".git" </> "config"

newtype GitConfig = GitConfig GCP.GitConfig deriving Eq

instance Show GitConfig where
    show = Text.unpack . showGitConfig

gitConfig :: (GLG2.MonadLg m, GLG2.HasLgRepo m) => m (Either String GitConfig)
gitConfig = do
    path <- gitConfigPath
    content <- liftIO $ TIO.readFile path
    return . bimap show GitConfig $ GCP.parseConfig content

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

-- | Assuming that we can correctly parse a gitconfig (because that's done by
-- an external library) this function ensures we can faithfully prettyprint
-- that content.
gitConfigSafe :: Either String GitConfig -> Either String GitConfig
gitConfigSafe original = do
    parsed <- original
    case bimap show GitConfig . GCP.parseConfig $ showGitConfig parsed of
        Left err -> Left $ "internal error: Failed to parse git-config after pretty-printing: " ++ err
        Right reparsed
            | parsed == reparsed -> original
            | otherwise -> Left "internal error: Pretty-printer failed to faithfully represent git-config"
