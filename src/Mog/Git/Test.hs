{-# OPTIONS_GHC "-Wno-unused-top-binds" #-}

{-# LANGUAGE TypeApplications #-}

-- | Test support for 'Mog.Git'. No exported functions.
module Mog.Git.Test () where

import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Proxy (Proxy(..))
import qualified Data.List as List (sort)
import qualified Git
import qualified Git.Libgit2 as GLG2
import qualified System.Process as Proc

import Mog.Example
import Mog.Git
import Mog.Output

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

withTestRepo :: ReaderT GLG2.LgRepo IO a -> IO a
withTestRepo action = do
    tmp <- init <$> Proc.readProcess "mktemp" ["--directory", "--tmpdir", "mog-test.XXXXXX"] ""
    Git.withNewRepository GLG2.lgFactory tmp
        -- XXX: withNewRepository deletes the test repo unless an exception occurs
        action

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
