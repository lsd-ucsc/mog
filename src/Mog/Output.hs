{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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

import Mog.Schema
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

-- | A column is either a single atom or group of columns.
data Col
    = Atom  ByteString
    | Group Row
    deriving (Show, Eq, Ord)
-- TODO Rename Col to Field

-- * Output an instance

toOutput :: (Convert a a' Datatype) => Proxy a -> a' -> Datatype
toOutput = convertTo

fromOutput :: (Convert a a' Datatype) => Proxy a -> Datatype -> Maybe a'
fromOutput = convertFrom



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


-- TODO: Replace Maybe with Either
class (Inst i ~ a) => Convert i a b where
    convertTo   :: Proxy i -> a -> b
    convertFrom :: Proxy i -> b -> Maybe a

instance (Convert ts ts' Datatype)
      => Convert (Schema name ts) ts' Datatype where
    convertTo   _ = convertTo   @ts Proxy
    convertFrom _ = convertFrom @ts Proxy

instance (Convert t t' Relation, Convert ts ts' Datatype, Named t)
      => Convert (t & ts) (t' :& ts') Datatype where
    convertTo _ (x :& xs) =
        Map.insert (named @t Proxy)
                   (convertTo @t  Proxy x)
                   (convertTo @ts Proxy xs)
    convertFrom _ tables =
        let name = named @t Proxy in
        liftA2 (:&) (Map.lookup name tables >>= convertFrom @t Proxy)
                    (convertFrom @ts Proxy (Map.delete name tables))

instance Convert TablesEnd TTablesEnd Datatype where
    convertTo   _ TTablesEnd = Map.empty
    convertFrom _ tables =
        if Map.null tables
            then Just TTablesEnd
            else Nothing

instance (Convert pk_v pk_v' Relation)
      => Convert (Table name pk_v) pk_v' Relation where
    convertTo   _ = convertTo   @pk_v Proxy
    convertFrom _ = convertFrom @pk_v Proxy

instance (Convert pk pk' Row, Convert v v' Row, RowWidth pk, Ord pk')
      => Convert (pk ↦ v) (Assoc pk' v') Relation where
--  convertTo _
--      = Map.mapWithKey (\k -> mappend k . convertTo @v Proxy)
--      . Map.mapKeys (convertTo @pk Proxy)
--  convertFrom _
--      -- Accumulate the pairs into a map
--      = fmap Map.fromList
--      -- Move from [Maybe _] to Maybe [_]
--      . sequenceA
--      -- Move from (Maybe _, Maybe _) to Maybe (_, _)
--      . map (uncurry (liftA2 (,)))
--      -- Convert both elements of each pair
--      . map (bimap (convertFrom @pk Proxy)
--                   (convertFrom @v Proxy . drop (rowWidth @pk Proxy)))
--      -- Stream the key-value pairs
--      . Map.toList

instance (Convert c c' Col, Convert cs cs' Row)
      => Convert (c % cs) (c' :% cs') Row where
    convertTo _ (c :% cs) = convertTo @c  Proxy c
                          : convertTo @cs Proxy cs
    convertFrom _ []     = Nothing
    convertFrom _ (x:xs) = liftA2 (:%) (convertFrom @c  Proxy x)
                                       (convertFrom @cs Proxy xs)

instance Convert Ø Ø_ Row where
    convertTo   _ Ø_    = []
    convertFrom _ []    = Just Ø_
    convertFrom _ (_:_) = Nothing

instance (Serialise a) => Convert (Prim a) a Col where
    convertFrom _ (Atom  x) = either (const Nothing) Just (deserialiseOrFail x)
    convertFrom _ (Group _) = Nothing
    convertTo   _           = Atom . serialise

instance (Convert fk fk' Row) => Convert (Ref fk index) fk' Col where
    convertFrom _ (Atom  _) = Nothing
    convertFrom _ (Group x) = convertFrom @fk Proxy x
    convertTo   _           = Group . convertTo @fk Proxy
