{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- TODO: !!!! Ord (Inst pk)

-- | Module for the datatype to which a schema instance is encoded or decoded
-- on the way to git
module Mog.Output where

import Control.Applicative (liftA2)
import Data.ByteString.Lazy (ByteString)
import Data.Bifunctor (bimap)
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


class RowWidth a where
    rowWidth :: Proxy a -> Int
instance RowWidth Ø where
    rowWidth _ = 0
instance (RowWidth cs) => RowWidth (c % cs) where
    rowWidth _ = 1 + rowWidth @cs Proxy


class Named a where
    named :: Proxy a -> String
instance (KnownSymbol name) => Named (Schema name ts) where
    named _ = symbolVal @name Proxy
instance (KnownSymbol name) => Named (Table name pk_v) where
    named _ = symbolVal @name Proxy


-- * Output an instance

toSchema :: (Convert a Datatype) => Proxy a -> Inst a -> Datatype
toSchema = convertTo

fromSchema :: (Convert a Datatype) => Proxy a -> Datatype -> Maybe (Inst a)
fromSchema = convertFrom

-- TODO: Replace Maybe with Either
class Convert a b where
    convertTo   :: Proxy a -> Inst a -> b
    convertFrom :: Proxy a -> b -> Maybe (Inst a)

instance (Convert ts Datatype)
      => Convert (Schema name ts) Datatype where
    convertTo   _ = convertTo   @ts Proxy
    convertFrom _ = convertFrom @ts Proxy

instance (Convert t Relation, Convert ts Datatype, Named t)
      => Convert (t & ts) Datatype where
    convertTo _ (x :& xs) =
        Map.insert (named @t Proxy)
                   (convertTo @t  Proxy x)
                   (convertTo @ts Proxy xs)
    convertFrom _ tables =
        let name = named @t Proxy in
        liftA2 (:&) (Map.lookup name tables >>= convertFrom @t Proxy)
                    (convertFrom @ts Proxy (Map.delete name tables))

instance Convert TablesEnd Datatype where
    convertTo   _ TTablesEnd = Map.empty
    convertFrom _ tables =
        if Map.null tables
            then Just TTablesEnd
            else Nothing

instance (Convert pk_v Relation) => Convert (Table name pk_v) Relation where
    convertTo   _ = convertTo   @pk_v Proxy
    convertFrom _ = convertFrom @pk_v Proxy

-- TODO: Ord (Inst pk) requires UndecidableInstances
instance (Convert pk Row, Convert v Row, Ord (Inst pk), RowWidth pk)
      => Convert (pk ↦ v) Relation where
    convertTo _
        = Map.mapWithKey (\k -> mappend k . convertTo @v Proxy)
        . Map.mapKeys (convertTo @pk Proxy)
    convertFrom _
        -- Accumulate the pairs into a map
        = fmap Map.fromList
        -- Move from [Maybe _] to Maybe [_]
        . sequenceA
        -- Move from (Maybe _, Maybe _) to Maybe (_, _)
        . map (uncurry (liftA2 (,)))
        -- Convert both elements of each pair
        . map (bimap (convertFrom @pk Proxy)
                     (convertFrom @v Proxy . drop (rowWidth @pk Proxy)))
        -- Stream the key-value pairs
        . Map.toList

instance (Convert c Col, Convert cs Row) => Convert (c % cs) Row where
    convertTo _ (c :% cs) = convertTo @c Proxy c : convertTo @cs Proxy cs
    convertFrom _ []     = Nothing
    convertFrom _ (x:xs) = liftA2 (:%) (convertFrom @c  Proxy x)
                                       (convertFrom @cs Proxy xs)

instance Convert Ø Row where
    convertTo   _ Ø_    = []
    convertFrom _ []    = Just Ø_
    convertFrom _ (_:_) = Nothing

instance (Serialise a) => Convert (Schema.Prim a) Col where
    convertFrom _ (Prim x) = either (const Nothing) Just (deserialiseOrFail x)
    convertFrom _ (Ref  _) = Nothing
    convertTo   _          = Prim . serialise

instance (Convert fk Row) => Convert (Schema.Ref fk index) Col where
    convertFrom _ (Prim _) = Nothing
    convertFrom _ (Ref  x) = convertFrom @fk Proxy x
    convertTo   _          = Ref . convertTo @fk Proxy
