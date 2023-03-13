{-# LANGUAGE FlexibleContexts #-}

-- | Module for interacting with a git backend to output/input a
-- 'Output.Database' value to/from disk
--
-- The functions in this module are monomorphic over the "gitlib" backend;
-- specifically they use the "gitlib-libgit2" backend.
module Mog.GitLibgit where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified Data.Text as Text hiding (Text)
import qualified Data.Text.IO as TIO

import qualified Git
import qualified Git.Libgit2 as GLG2

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
