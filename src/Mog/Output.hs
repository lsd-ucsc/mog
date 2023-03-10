{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Module for the datatype to which a schema instance is encoded or decoded
-- on the way to git
module Mog.Output where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Map as Map

import Codec.Serialise (Serialise, serialise)

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
