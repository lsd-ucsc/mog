{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module for interacting with a git backend to output/input a
-- 'Output.Database' value to/from disk
--
-- The naming convention in here is currently following that of the Output module.
module Mog.Git where

-- import Control.Exception (bracket)
-- import Git.Libgit2 (LgRepo, lgFactory)
import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Control.Arrow (first, second)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Map as Map

import Crypto.Hash (hashlazy, Digest, SHA1)
import qualified Git

import qualified Mog.Output as Output

data Col r
    = Prim (Git.BlobOid r)
    | Ref (Git.TreeOid r)

-- TODO: turn on the attribute
--
-- cat .git/info/attributes
-- * merge=union

-- XXX: This `withRepo` requires UnliftIO constraints on n and m and I don't
-- want to depend on those packages
--
--withRepo :: ... => Git.RepositoryFactory n m r -> FilePath -> n a -> m a
--withRepo backend path action =
--    Git.withRepository'
--        backend
--        Git.RepositoryOptions
--            { Git.repoPath = path
--            , Git.repoIsBare = True
--            , Git.repoAutoCreate = True
--            , Git.repoWorkingDir = Nothing
--            }
--        action

-- TODO: consider how to store an Output.Database w/o erasing the other trees
-- (Output.Datatype) in the same database
--
-- TODO: storeDatabase :: Git.MonadGit r m => 

storeDatatype :: Git.MonadGit r m => Output.Datatype -> m (Git.TreeOid r)
storeDatatype
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRelation)
    .   fmap (first $ pack) -- FIXME: Packing a user-provided string here; should instead go via Text's encodeUtf8
    .   Map.toList

-- | Create a tree OID containing, for each row, a hash of the key mapped to an
-- OID for the value. (Assumption: Key data is already in the value.)
storeRelation :: Git.MonadGit r m => Output.Relation -> m (Git.TreeOid r)
storeRelation
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRow)
    .   fmap (first $ pack . show . hashRow) -- XXX: packing a hex-digest of a hash, so Char8.pack should be safe
    .   Map.toList

hashRow :: Output.Row -> Digest SHA1
hashRow = hashlazy . mconcat . hashChunksRow

hashChunksRow :: Output.Row -> [ByteString]
hashChunksRow = concatMap hashChunksCol

hashChunksCol :: Output.Col -> [ByteString]
hashChunksCol (Output.Prim bs) = [bs]
hashChunksCol (Output.Ref row) = hashChunksRow row

-- | Create a tree OID containing, for each column, a tuple-index mapped to an OID.
storeRow :: Git.MonadGit r m => Output.Row -> m (Git.TreeOid r)
storeRow
    =   Git.createTree
    .   mapM putCol
    <=< return
    .   fmap (first $ pack . ('t':) . show) -- XXX: packing a single 't' followed by digits, so Char8.pack should be safe
    .   zip [0..]
    <=< mapM storeCol
  where
    putCol (name, col) = case col of
        Prim bid -> Git.putBlob name bid
        Ref tid -> Git.putTree name tid

-- | Create an OID for one column.
storeCol :: Git.MonadGit r m => Output.Col -> m (Col r)
storeCol (Output.Prim bs) = return . Prim =<< Git.createBlob (Git.BlobStringLazy bs)
storeCol (Output.Ref row) = return . Ref =<< storeRow row
