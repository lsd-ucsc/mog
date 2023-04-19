{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Mog.MergeDriver.Merge where

import Control.Applicative (liftA3)
import Codec.Serialise (Serialise, serialise, deserialiseOrFail, DeserialiseFailure)
import Control.Exception (Exception)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol)
import System.FilePath (splitPath, dropTrailingPathSeparator)
import qualified Data.Text as Text hiding (Text)

import Mog.Schema
import Mog.Output (Named(..), RowWidth(..))
import Mog.Git (LoadError, parseFieldName)

class Mergeable a where
    -- | @merge base ours theirs@ is a family of binary merges against a
    -- base. Any @merge base@ is a commutative semigroup for
    -- versions that have @base@ as an ancestor.
    --
    -- (TODO: It might be a monoid but we'll think about the identity later.)
    merge :: a -> (a -> a -> a)

data MergeError
    = UnexpectedPathComponents FilePath
    | WrongDatatypeName {got::Text, expected::Text}
    | RelationNotFound {datatype::Text, relation::Text}
    | FieldNotFound {datatype::Text, relation::Text, field::Text}
    | InvalidFilename LoadError
    | DeserialiseFailure DeserialiseFailure
    | UnexpectedError{reason::String}
    deriving Show
instance Exception MergeError

data SchemaPath = SchemaPath
    { dtName :: Text
    , relName :: Text
--  , tupName :: Text
    , fieldName :: Text
    , fieldIx :: Int
    } deriving Show

-- |
--
-- >>> schemaPath "ordered-map/map/0c11d463c749db5838e2c0e489bf869d531e5403.tup/t1.val"
-- Right (SchemaPath {dtName = "ordered-map", relName = "map", fieldName = "t1.val", fieldIx = 1})
schemaPath :: FilePath -> Either MergeError SchemaPath
schemaPath fp =
    case Text.pack <$> split fp of
        [dtName, relName, _tupName, fieldName] -> do
            fieldIx <- bimap InvalidFilename fst $ parseFieldName fieldName
            return SchemaPath{..}
        _ -> Left $ UnexpectedPathComponents fp
  where
    split = map dropTrailingPathSeparator . splitPath

type CBOR = ByteString
type BasedMerge = CBOR -> Merge
type Merge = CBOR -> CBOR -> Either MergeError CBOR

-- | @ordered-map/map/0c11d463c749db5838e2c0e489bf869d531e5403.tup/t1.val@
class FindAndMerge s where
    findMerge :: Proxy s -> SchemaPath -> BasedMerge

instance (KnownSymbol name, FindAndMerge tables) =>
    FindAndMerge (Schema name tables) where
    findMerge ir sp@SchemaPath{dtName=n}
        | n == named ir = findMerge @tables Proxy sp
        | otherwise = \_ _ _ -> Left WrongDatatypeName{got=n, expected=named ir}

-- | Slightly sketchy direct use of 'Named' on @t@ here without checking that
-- it's constructed with 'Table'. This avoids us needing to actually call
-- merged on @t@ and then examine the result before deciding to call it on
-- @ts@. Note that this means we require recursive class instances in both the
-- head and tail. Very sketchy.
instance (Named t, FindAndMerge t, FindAndMerge ts) =>
    FindAndMerge (t & ts) where
    findMerge _ir sp@SchemaPath{relName=n}
        | n == named @t Proxy = findMerge @t Proxy sp
        | otherwise = findMerge @ts Proxy sp
instance
    FindAndMerge TablesEnd where
    findMerge _ir SchemaPath{dtName=datatype, relName=relation} =
        \_ _ _ -> Left RelationNotFound{..}

-- | We check the name matches here a second time. Oh well. Nothing is perfect.
instance (KnownSymbol name, FindAndMerge pk_v) =>
    FindAndMerge (Table name pk_v) where
    findMerge ir sp@SchemaPath{relName=n}
        | n == named ir = findMerge @pk_v Proxy sp
        | otherwise = error "unreachable: the name should have been validated in the instance for (t & ts)"

-- | We skip the primary keys because those are non-mergeable types.
instance (FindAndMerge v, RowWidth pk) =>
    FindAndMerge (pk ↦ v) where
    findMerge _ir sp@SchemaPath{fieldIx=ix} =
        findMerge @v Proxy sp{fieldIx=ix - rowWidth @pk Proxy}

-- | Similar to the search through @t & ts@ above, here we search through @c %
-- cs@ by counting down the 'fieldIx' to 0. Again, this requires recursive
-- class instances in both the head and tail.
instance (FindAndMerge c, FindAndMerge cs) =>
    FindAndMerge (c % cs) where
    findMerge _ir sp@SchemaPath{fieldIx=ix}
        | 0  < ix   = findMerge @cs Proxy sp{fieldIx=ix - 1}
        | 0 == ix   = findMerge @c Proxy sp
        | otherwise = \_ _ _ -> Left UnexpectedError{reason="findMerge: negative fieldIx=" ++ show ix ++"; might result from an improper schema change"}
instance
    FindAndMerge Ø where
    findMerge _ir SchemaPath{dtName=datatype, relName=relation, fieldName=field} =
        \_ _ _ -> Left FieldNotFound{..}

instance (Mergeable a, Serialise a) =>
    FindAndMerge (Prim a) where
    findMerge _ir _sp base ours theirs =
        let deserialise = bimap DeserialiseFailure id . deserialiseOrFail in
        serialise <$> liftA3 (merge @a)
            (deserialise base)
            (deserialise ours)
            (deserialise theirs)
