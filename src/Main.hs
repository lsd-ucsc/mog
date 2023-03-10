#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskell.packages.ghc810.ghcWithPackages (p: with p; [gitlib (haskell.lib.markUnbroken gitlib-libgit2) hlibgit2 cryptonite serialise memory])"

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
    , TreeOid
    )
import qualified Git
import Git.Libgit2
    ( LgRepo
    , lgFactory
    )
import Control.Monad.IO.Class
    ( MonadIO(..)
    )
import Data.Set
    ( Set
    )
import Codec.Serialise
    ( Serialise
    , serialise
    , deserialiseOrFail
    )

import Mog
import Example

main :: IO ()
main = do
    Git.withRepository'
        lgFactory
        Git.defaultRepositoryOptions
            { repoPath = "store.git"
            , repoIsBare = True
            , repoAutoCreate = True
            }
        repoMain

repoMain :: (MonadGit LgRepo m, MonadIO m) => m ()
repoMain = do
    ref <- Git.resolveReference "HEAD"
    liftIO $ print ("HEAD", ref)
    -- liftIO $ print =<< currentTree
    -- TODO: turn on the attribute
    --
    -- cat .git/info/attributes
    -- * merge=union
    make3Tuple (("goodbye", 123, 1.5) :: (String, Int, Float))
    return ()

--class Tuple a where
--
--instance Tuple (a, b) where

newtype TupleOid r = TupleOid (TreeOid r)

make3Tuple :: (MonadGit r m, Serialise a, Serialise b, Serialise c) =>
    (a, b, c) -> m (TupleOid r)
make3Tuple (x1, x2, x3) = do
    b1 <- Git.createBlob . Git.BlobStringLazy . serialise $ x1
    b2 <- Git.createBlob . Git.BlobStringLazy . serialise $ x2
    b3 <- Git.createBlob . Git.BlobStringLazy . serialise $ x3
    tree <- Git.createTree $ do
        Git.putBlob "t1" b1
        Git.putBlob "t2" b2
        Git.putBlob "t3" b3
    return $ TupleOid tree

-- makeTree :: MonadGit r m => Set (a, b) -> m (TreeOid r)
-- makeTree xs = do
--     Git.createTree $ do
--         
