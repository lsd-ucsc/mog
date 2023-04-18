{-# OPTIONS_GHC "-Wno-unused-top-binds" #-}

{-# LANGUAGE TypeApplications #-}

-- | Test support for 'Mog.Git'. No exported functions.
module Mog.Git.Test () where

import Control.Exception (bracket)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Proxy (Proxy(..))
import System.FilePath ((</>))
import qualified Data.List as List (sort)
import qualified System.Directory as Dir
import qualified System.Process as Proc

import qualified Git
import qualified Git.Libgit2 as GLG2

import Mog.Example
import Mog.Git
import Mog.Git.Mutex
import Mog.Output

-- $setup
-- >>> import qualified Control.Concurrent.Async as Async




-- * Database round-trip test

example :: [Datatype]
example = [q, m]
  where
    q = toOutput @(QueueSchema Int) Proxy queueInstance
    m = toOutput @(OrderedMapSchema Char Int) Proxy orderedMapExample

sort :: [Datatype] -> [Datatype]
sort = List.sort . map (fmap sortR)
  where
    sortR :: [Relation] -> [Relation]
    sortR = List.sort . map (fmap sortT)
    sortT :: [Tuple] -> [Tuple]
    sortT = List.sort . map (fmap List.sort)

eq :: [Datatype] -> [Datatype] -> Bool
eq a b = sort a == sort b

-- |
--
-- >>> result <- roundtrip example
-- >>> example' <- either (fail . show) return result
-- >>> eq example example'
-- True
--
roundtrip :: [Datatype] -> IO (Either LoadError [Datatype])
roundtrip db =
    withTestRepo $ do
        runExceptT . loadDatabase =<< storeDatabase db




-- * Symlink mutex

-- |
--
-- Happy path.
--
-- >>> testPidSymlink (\_ -> print "before") (\_ -> print "in" >> return "out")
-- "before"
-- "in"
-- Right "out"
--
-- Already-exists error.
--
-- >>> testPidSymlink (flip writeFile "") (\_ -> print "in" >> return "out")
-- (path persists)
-- Left (PidSymlinkPathAlreadyExists {exc = ...})
--
-- >>> testPidSymlink Dir.createDirectory (\_ -> print "in" >> return "out")
-- (path persists)
-- Left (PidSymlinkPathAlreadyExists {exc = ...})
--
-- Cleanup-error is printed, not returned.
--
-- >>> testPidSymlink (\_ -> print "before") (\p -> Dir.removeFile p >> return "out")
-- "before"
-- PidSymlinkMissingOnCleanup {exc = ...}
-- Right "out"
--
-- If you run the following program enough times you might see
-- 'PidSymlinkRaceLost', but I think it's rare enough (or impossible under
-- doctest evaluation?) that this test isn't flaky.
--
-- >>> :{
-- withTmpDir "link" $ \dir -> do
--     let link = dir </> "pid.link"
--     Async.concurrently
--         (Proc.callProcess "ln" ["-s", "bogus", link])
--         (withPidSymlink link $ return "ok")
-- :}
-- ((),Right "ok")
--
testPidSymlink :: Show a => (FilePath -> IO ()) -> (FilePath -> IO a) -> IO (Either PidSymlinkError a)
testPidSymlink before inner =
    withTmpDir "link" $ \dir -> do
        let link = dir </> "pid.link"
        before link
        result <- withPidSymlink link (inner link)
        exists <- Dir.doesPathExist link
        if exists then putStrLn "(path persists)" else return ()
        return result




-- * Helpers

withTestRepo :: ReaderT GLG2.LgRepo IO a -> IO a
-- | Deletes the test repo unless an exception occurs.
withTestRepo action = do
    dir <- init <$> Proc.readProcess "mktemp" ["--directory", "--tmpdir", "mog-test.repo.XXXXXXX"] ""
    Git.withNewRepository GLG2.lgFactory dir
        action

withTmpDir :: String -> (FilePath -> IO a) -> IO a
withTmpDir tag =
    bracket
        (init <$> Proc.readProcess "mktemp" ["--directory", "--tmpdir", "mog-test." ++ tag ++ ".XXXXXXX"] "")
        Dir.removeDirectoryRecursive
