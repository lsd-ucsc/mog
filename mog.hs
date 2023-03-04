#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskell.packages.ghc8107.ghcWithPackages (p: [p.gitlib (p.gitlib-libgit2.overrideAttrs (old: {meta={broken=false;};})) p.hlibgit2])"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, MonoLocalBinds #-} -- MonadGit based type sig

-- import System.Environment (getArgs)
-- import System.Process (callProcess)
-- import System.Directory (withCurrentDirectory, createDirectory)
-- import Data.Set

    -- callProcess "git" ["init", repo]
    -- withCurrentDirectory repo repoMain
    -- createDirectory

import Git
    ( MonadGit
    , RepositoryOptions(..)
    , defaultRepositoryOptions
    , withRepository'
    , resolveReference
    )
import Git.Libgit2
    ( LgRepo
    , lgFactory
    )
import Control.Monad.IO.Class
    ( MonadIO(..)
    )

main :: IO ()
main = do
    withRepository'
        lgFactory
        defaultRepositoryOptions
            { repoPath = "store.git"
            , repoIsBare = True
            , repoAutoCreate = True
            }
        repoMain

repoMain :: (MonadGit LgRepo m, MonadIO m) => m ()
repoMain = do
    ref <- resolveReference "HEAD"
    -- TODO: turn on the attribute
    --
    -- cat .git/info/attributes
    -- * merge=union
    liftIO $ print ref
    return ()
