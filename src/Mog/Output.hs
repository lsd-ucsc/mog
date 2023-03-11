{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

-- TODO Rename ToSchema to ToDatatype
class ToSchema a where
    toSchema :: Proxy a -> Inst a -> (String, Datatype)
instance (ToTables ts, KnownSymbol name) => ToSchema (Schema name ts) where
    toSchema Proxy x = (symbolVal @name Proxy, Map.fromList (toTables @ts Proxy x))

-- TODO Rename ToTables to ToRelations
class ToTables x where
    toTables :: Proxy x -> Inst x -> [(String, Relation)]
instance (ToTable t, ToTables ts) => ToTables (t & ts) where
    toTables Proxy (x :& xs) = toTable @t Proxy x : toTables @ts Proxy xs
instance ToTables TablesEnd where
    toTables Proxy TTablesEnd = []

-- TODO Rename ToTable to ToRelation
class ToTable x where
    toTable :: Proxy x -> Inst x -> (String, Relation)
instance (ToColumns pk_v, KnownSymbol name) => ToTable (Table name pk_v) where
    toTable Proxy x = (symbolVal @name Proxy, toColumns @pk_v Proxy x)

-- TODO Rename ToColumns to ToRows
class ToColumns x where
    toColumns :: Proxy x -> Inst x -> Relation
instance (ToFields pk, ToFields v) => ToColumns (pk ↦ v) where
    toColumns Proxy = Map.mapWithKey (\k -> mappend k . toFields @v Proxy)
                    . Map.mapKeys (toFields @pk Proxy)

-- TODO Rename ToFields to ToRow
class ToFields a where
    toFields :: Proxy a -> Inst a -> Row
instance (ToField c, ToFields cs) => ToFields (c % cs) where
    toFields Proxy (c :% cs) = toField @c Proxy c : toFields @cs Proxy cs
instance ToFields Ø where
    toFields Proxy TTupleEnd = []

class ToField a where
    toField  :: Proxy a -> Inst a -> Col
instance (Serialise a) => ToField (Schema.Prim a) where
    toField Proxy = Prim . serialise
instance (ToFields fk) => ToField (Schema.Ref fk index) where
    toField Proxy = Ref . toFields @fk Proxy



-- TODO: Replace Maybe with Either

class FromSchema a where
    schemaName :: Proxy a -> String
    fromSchema :: Proxy a -> Datatype -> Maybe (Inst a)
instance (FromTables ts, KnownSymbol name) => FromSchema (Schema name ts) where
    schemaName Proxy    = symbolVal @name Proxy
    fromSchema Proxy dt = fromTables @ts Proxy dt

class FromTables a where
    fromTables :: Proxy a -> Datatype -> Maybe (Inst a)
instance (FromTable t, FromTables ts) => FromTables (t & ts) where
    fromTables Proxy tables = do
        let name = tableName @t Proxy
        head <- Map.lookup name tables >>= fromTable @t Proxy
        tail <- fromTables @ts Proxy (Map.delete name tables)
        Just (head :& tail)
instance FromTables TablesEnd where
    fromTables Proxy tables =
        if Map.null tables
            then Just TTablesEnd
            else Nothing

class FromTable a where
    tableName :: Proxy a -> String
    fromTable :: Proxy a -> Relation -> Maybe (Inst a)
instance (FromColumns pk_v, KnownSymbol name) => FromTable (Table name pk_v) where
    tableName Proxy   = symbolVal @name Proxy
    fromTable Proxy r = fromColumns @pk_v Proxy r

class FromColumns a where
    fromColumns :: Proxy a -> Relation -> Maybe (Inst a)
instance (FromFields pk, FromFields v) => FromColumns (pk ↦ v) where
    fromColumns Proxy m = _fromColumns
    {-
    Goal: Maybe (Map (Inst pk) (Inst v))
    -----
    Have: m :: Map Row Row
          pk :: ???
          v :: ???
    -}

class FromFields a where
    fromFields :: Proxy a -> Row -> Maybe (Inst a)
instance FromFields Ø where
    fromFields Proxy []     = Just TTupleEnd
    fromFields Proxy (_:_)  = Nothing
instance (FromField c, FromFields cs) => FromFields (c % cs) where
    fromFields Proxy []     = Nothing
    fromFields Proxy (x:xs) = liftA2 (:%) (fromField  @c  Proxy x)
                                          (fromFields @cs Proxy xs)

class FromField a where
    fromField  :: Proxy a -> Col -> Maybe (Inst a)
instance (Serialise a) => FromField (Schema.Prim a) where
    fromField Proxy (Prim x) = either (const Nothing) Just (deserialiseOrFail x)
    fromField Proxy (Ref  x) = Nothing
instance (FromFields fk) => FromField (Schema.Ref fk ix) where
    fromField Proxy (Prim x) = Nothing
    fromField Proxy (Ref  x) = fromFields @fk Proxy x
