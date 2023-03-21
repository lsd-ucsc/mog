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

import Control.Arrow (first, second)
import Control.Monad ((<=<))
import qualified Data.ByteString.Char8 as Char8 (pack)
import qualified Data.Text as Text (pack)
import qualified Data.Text.Encoding as Text (encodeUtf8)

import qualified Git

import qualified Mog.Output as Output

data Field r
    = Atom  (Git.BlobOid r)
    | Group (Git.TreeOid r)

-- TODO: consider how to store an Output.Database w/o erasing the other trees
-- (Output.Datatype) in the same database
--
-- TODO: storeDatabase :: Git.MonadGit r m => 

storeDatatype :: Git.MonadGit r m => Output.Datatype -> m (Git.TreeOid r)
storeDatatype
    -- TODO: We can control the merging of keys and values with suffixes:
    --   .gitattributes := ```
    --   *.pk merge=binary
    --   *.val merge=custom-driver
    --   *.blah merge=blah-driver # hypothetically we might want to give users the option to use existing merge drivers
    --   ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRelation)
    .   fmap (first $ Text.encodeUtf8 . Text.pack)
    .   snd -- FIXME: currently we just throw out the datatype name

-- | Create a tree OID containing, for each row, a hash of the key mapped to an
-- OID for the value. (Assumption: Key data is already in the value.)
storeRelation :: Git.MonadGit r m => [Output.Tuple] -> m (Git.TreeOid r)
storeRelation
    -- TODO: As an alternative to placing these configs at the datatype level,
    -- we can place them here by outputting the names of tuple columns:
    --   .gitattributes := ```
    --   */{pkfilecolumns} merge=binary
    --   */{valuecolumns} merge=custom-driver
    --   ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRow)
    .   fmap (first $ Char8.pack . show) -- XXX: packing a hex-digest of a hash, so Char8.pack should be safe

-- | Create a tree OID containing, for each column, a tuple-index mapped to an OID.
storeRow :: Git.MonadGit r m => Output.Row -> m (Git.TreeOid r)
storeRow
    -- TODO: What if a row contains a foreign key? Those are subdirectories.
    -- Foreign keys are always referring to primary keys which don't merge
    -- (merge=binary).
    =   Git.createTree
    .   mapM putCol
    <=< return
    .   fmap (first $ Char8.pack . ('t':) . show) -- XXX: packing a single 't' followed by digits, so Char8.pack should be safe
    .   zip [0::Int ..]
    <=< mapM storeCol
  where
    putCol (name, col) = case col of
        Atom  bid -> Git.putBlob name bid
        Group tid -> Git.putTree name tid

-- | Create an OID for one column.
storeCol :: Git.MonadGit r m => Output.Col -> m (Field r)
storeCol (Output.Atom bs) = Atom <$> Git.createBlob (Git.BlobStringLazy bs)
storeCol (Output.Group row) = Group <$> storeRow row
