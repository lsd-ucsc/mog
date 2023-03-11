{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- TODO: !!!! Ord (Inst pk)

-- | Module for the datatype to which a schema instance is encoded or decoded
-- on the way to git
module Mog.Output where

import Control.Applicative (liftA2)
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Map as Map

import Codec.Serialise (Serialise, serialise, deserialiseOrFail)

import Mog.Schema hiding (Prim, Ref) -- FIXME: these names collide
import qualified Mog.Schema as Schema (Prim, Ref)
import Mog.Instance




-- * Git datatype

-- | Multiple datatypes associated with their characteristic relations.
type Database = Map String Datatype

-- | A datatype represented as a group of named characteristic relations.
type Datatype = Map String Relation

-- | A characteristic relation represented as a group of db-tuples.
--
--  * The key includes only the columns in the PK.
--  * The value includes all the columns.
type Relation = Map Row Row

-- | CBOR encoded columns representing a db-tuple.
type Row = [Col]

-- | A column is either a primitive or a reference to columns of another table.
data Col
    = Prim ByteString
    | Ref Row
    deriving (Show, Eq, Ord)
-- TODO Rename Col to Field




-- * Output an instance

-- TODO: Replace Maybe with Either

-- TODO Rename ToSchema to ToDatatype
class ToSchema a where
    schemaName :: Proxy a -> String
    fromSchema :: Proxy a -> Datatype -> Maybe (Inst a)
    toSchema   :: Proxy a -> Inst a   -> Datatype
instance (ToTables ts, KnownSymbol name) => ToSchema (Schema name ts) where
    schemaName Proxy    = symbolVal @name Proxy
    fromSchema Proxy dt = fromTables @ts Proxy dt
    toSchema   Proxy x  = Map.fromList (toTables @ts Proxy x)

-- TODO Rename ToTables to ToRelations
class ToTables a where
    fromTables :: Proxy a -> Datatype -> Maybe (Inst a)
    toTables   :: Proxy a -> Inst a   -> [(String, Relation)]
instance (ToTable t, ToTables ts) => ToTables (t & ts) where
    fromTables Proxy tables =
        let name = tableName @t Proxy in
        liftA2 (:&) (Map.lookup name tables >>= fromTable @t Proxy)
                    (fromTables @ts Proxy (Map.delete name tables))
    toTables Proxy (x :& xs) = (tableName @t Proxy , toTable @t Proxy x)
                             : toTables @ts Proxy xs
instance ToTables TablesEnd where
    fromTables Proxy tables =
        if Map.null tables
            then Just TTablesEnd
            else Nothing
    toTables Proxy TTablesEnd = []

-- TODO Rename ToTable to ToRelation
class ToTable a where
    tableName :: Proxy a -> String
    fromTable :: Proxy a -> Relation -> Maybe (Inst a)
    toTable   :: Proxy a -> Inst a   -> Relation
instance (ToColumns pk_v, KnownSymbol name) => ToTable (Table name pk_v) where
    tableName Proxy   = symbolVal @name Proxy
    fromTable Proxy r = fromColumns @pk_v Proxy r
    toTable   Proxy x = toColumns @pk_v Proxy x

-- TODO Rename ToColumns to ToRows
-- TODO: Ord (Inst pk) requires UndecidableInstances
class ToColumns a where
    fromColumns :: Proxy a -> Relation -> Maybe (Inst a)
    toColumns   :: Proxy a -> Inst a   -> Relation
instance (ToFields pk, ToFields v, Ord (Inst pk), RowWidth pk) => ToColumns (pk ↦ v) where
    fromColumns Proxy m
        = fmap Map.fromList
        . sequenceA
        . map (\(x, y) -> liftA2 (,) (fromFields @pk Proxy x)
                                     (fromFields @v Proxy $ drop (rowWidth @pk Proxy) y))
        $ Map.toList m
    toColumns Proxy = Map.mapWithKey (\k -> mappend k . toFields @v Proxy)
                    . Map.mapKeys (toFields @pk Proxy)

-- TODO Rename ToFields to ToRow
class ToFields a where
    fromFields :: Proxy a -> Row    -> Maybe (Inst a)
    toFields   :: Proxy a -> Inst a -> Row
instance (ToField c, ToFields cs) => ToFields (c % cs) where
    fromFields Proxy []     = Nothing
    fromFields Proxy (x:xs) = liftA2 (:%) (fromField  @c  Proxy x)
                                          (fromFields @cs Proxy xs)
    toFields Proxy (c :% cs) = toField @c Proxy c : toFields @cs Proxy cs
instance ToFields Ø where
    fromFields Proxy []     = Just TTupleEnd
    fromFields Proxy (_:_)  = Nothing
    toFields Proxy TTupleEnd = []

class ToField a where
    fromField :: Proxy a -> Col    -> Maybe (Inst a)
    toField   :: Proxy a -> Inst a -> Col
instance (Serialise a) => ToField (Schema.Prim a) where
    fromField Proxy (Prim x) = either (const Nothing) Just (deserialiseOrFail x)
    fromField Proxy (Ref  x) = Nothing
    toField Proxy = Prim . serialise
instance (ToFields fk) => ToField (Schema.Ref fk index) where
    fromField Proxy (Prim x) = Nothing
    fromField Proxy (Ref  x) = fromFields @fk Proxy x
    toField Proxy = Ref . toFields @fk Proxy


class RowWidth a where
    rowWidth :: Proxy a -> Int
instance RowWidth Ø where
    rowWidth Proxy = 0
instance (RowWidth cs) => RowWidth (c % cs) where
    rowWidth Proxy = 1 + rowWidth @cs Proxy
