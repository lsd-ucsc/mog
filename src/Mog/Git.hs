{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module for interacting with a git backend to output/input a
-- 'Output.Database' value to/from disk
--
-- The functions in this module are polymorphic over the "gitlib" backend.
--
-- The naming convention in here is currently following that of the Output module.
module Mog.Git where

-- import Control.Exception (bracket)
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

-- TODO: consider how to store an Output.Database w/o erasing the other trees
-- (Output.Datatype) in the same database
--
-- TODO: storeDatabase :: Git.MonadGit r m => 

storeDatatype :: Git.MonadGit r m => Output.Datatype -> m (Git.TreeOid r)
storeDatatype
    -- TODO: We can control the merging of keys and values with suffixes:
    -- .gitattributes := ```
    -- *.pk merge=binary
    -- *.val merge=custom-driver
    -- *.blah merge=blah-driver # hypothetically we might want to give users the option to use existing merge drivers
    -- ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRelation)
    .   fmap (first $ pack) -- FIXME: Packing a user-provided string here; should instead go via Text's encodeUtf8
    .   Map.toList

-- | Create a tree OID containing, for each row, a hash of the key mapped to an
-- OID for the value. (Assumption: Key data is already in the value.)
storeRelation :: Git.MonadGit r m => Output.Relation -> m (Git.TreeOid r)
storeRelation
    -- TODO: As an alternative to placing these configs at the datatype level,
    -- we can place them here by outputting the names of tuple columns:
    -- .gitattributes := ```
    -- */{pkfilecolumns} merge=binary
    -- */{valuecolumns} merge=custom-driver
    -- ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRow)
    .   fmap (first $ pack . show . hashRow) -- XXX: packing a hex-digest of a hash, so Char8.pack should be safe
    .   Map.toList

hashRow :: Output.Row -> Digest SHA1
hashRow = hashlazy . hashInputRow

hashInputRow :: Output.Row -> ByteString
hashInputRow = mconcat . fmap hashChunksCol

hashChunksCol :: Output.Col -> ByteString
hashChunksCol (Output.Atom bs) = bs
hashChunksCol (Output.Group row) = hashInputRow row

-- | Create a tree OID containing, for each column, a tuple-index mapped to an OID.
storeRow :: Git.MonadGit r m => Output.Row -> m (Git.TreeOid r)
storeRow
    -- TODO: What if a row contains a foreign key? Those are subdirectories.
    -- Foreign keys are always referring to primary keys which don't merge
    -- (merge=binary).
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
storeCol (Output.Atom bs) = Prim <$> Git.createBlob (Git.BlobStringLazy bs)
storeCol (Output.Group row) = Ref <$> storeRow row
