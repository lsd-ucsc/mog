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
import Control.Monad ((<=<))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Codec.Serialise (Serialise, serialise, deserialiseOrFail, DeserialiseFailure)
import Crypto.Hash (hashlazy, Digest, SHA1)

import Mog.Schema
import Mog.Instance

-- $setup
-- >>> import Mog.Example




-- * Git datatype

-- | A datatype represented as a group of named characteristic relations.
type Datatype = (String, [Relation])

-- | A characteristic relation represented as a group of db-tuples.
type Relation = (String, [Tuple])

-- | A tuple containing the hash of PK columns and CBOR of all the columns.
type Tuple = (Digest SHA1, Row)

-- | CBOR encoded columns representing a db-tuple.
type Row = [Col]

-- | A column is either a single atom or group of columns.
data Col
    = Atom  ByteString
    | Group Row
    deriving Show
-- TODO Rename Col to Field




-- * Output an instance

toOutput :: (Convert a a' Datatype) => Proxy a -> a' -> Datatype
toOutput = convertTo

fromOutput :: (Convert a a' Datatype) => Proxy a -> Datatype -> Option a'
fromOutput = convertFrom




-- ** Helpers

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

hashRow :: Row -> Digest SHA1
hashRow = hashlazy . hashInputRow

hashInputRow :: Row -> ByteString
hashInputRow = mconcat . fmap hashChunksCol

hashChunksCol :: Col -> ByteString
hashChunksCol (Atom bs) = bs
hashChunksCol (Group row) = hashInputRow row




-- ** Storage class

class (Inst i ~ a) => Convert i a b where
    convertTo   :: Proxy i -> a -> b
    convertFrom :: Proxy i -> b -> Option a

data RestoreError
    = WrongDatatypeName {got::String, expected::String}
    | WrongRelationName {got::String, expected::String}
    | TooFewRelations {expected::String}
    | TooManyRelations {unexpected::String}
    | TooFewColumns
    | TooManyColumns
    | WrongPkHash {gotHash::Digest SHA1, expectedHash::Digest SHA1}
    | GotGroup'ExpectedAtom
    | GotAtom'ExpectedGroup
    | DeserialiseFailure DeserialiseFailure
    deriving (Eq, Show)

type Option = Either RestoreError

-- | A named datatype with a tuple of relations
--
-- >>> :{
-- pure orderedMapExample == convertFrom @(OrderedMapSchema Char Int) @_ @Datatype Proxy
--                            (convertTo @(OrderedMapSchema Char Int) @_ @Datatype Proxy orderedMapExample)
-- :}
-- True
--
instance (Convert ts ts' [Relation], KnownSymbol name)
      => Convert (Schema name ts) ts' Datatype where
    convertTo _ x =
        ( symbolVal @name Proxy
        , convertTo @ts Proxy x )
    convertFrom _ (n, x)
        | n == symbolVal @name Proxy = convertFrom @ts Proxy x
        | otherwise = Left WrongDatatypeName{got=n, expected=symbolVal @name Proxy}

instance (Convert t t' Relation, Convert ts ts' [Relation], Named t)
      => Convert (t & ts) (t' :& ts') [Relation] where
    convertTo _ (x :& xs) =
        convertTo @t Proxy x :
        convertTo @ts Proxy xs
    convertFrom _ tables =
        case tables of
            []   -> Left TooFewRelations{expected=named @t Proxy}
            x:xs -> liftA2 (:&) (convertFrom @t Proxy x)
                                (convertFrom @ts Proxy xs)

instance Convert TablesEnd TTablesEnd [Relation] where
    convertTo   _ TTablesEnd = []
    convertFrom _ tables =
        case tables of
            []      -> Right TTablesEnd
            (n,_):_ -> Left TooManyRelations{unexpected=n}

instance (Convert pk_v pk_v' [Tuple], KnownSymbol name)
      => Convert (Table name pk_v) pk_v' Relation where
    convertTo _ x =
        ( symbolVal @name Proxy
        , convertTo @pk_v Proxy x )
    convertFrom _ (n, x)
        | n == symbolVal @name Proxy = convertFrom @pk_v Proxy x
        | otherwise = Left WrongRelationName{got=n, expected=symbolVal @name Proxy}

-- |
--
-- Set of singleton tuples.
--
-- >>> type Eg0 = Prim String % Ø ↦ Ø
-- >>> eg0 = [("hello":%Ø_, Ø_), ("world":%Ø_, Ø_)]
-- >>> convertTo @Eg0 @_ @[Tuple] Proxy eg0
-- [(34...af,[Atom "ehello"]),(70...e2,[Atom "eworld"])]
-- >>> Right eg0 == convertFrom @Eg0 @_ @[Tuple] Proxy (convertTo @Eg0 @_ @[Tuple] Proxy eg0)
-- True
--
-- Set of pairs.
--
-- >>> type Eg1 = Prim String % Prim String % Ø ↦ Ø
-- >>> eg1 = [("a":%"bc":%Ø_, Ø_), ("ab":%"c":%Ø_, Ø_)]
-- >>> convertTo @Eg1 @_ @[Tuple] Proxy eg1
-- [(fb...d0,[Atom "aa",Atom "bbc"]),(5c...db,[Atom "bab",Atom "ac"])]
-- >>> Right eg1 == convertFrom @Eg1 @_ @[Tuple] Proxy (convertTo @Eg1 @_ @[Tuple] Proxy eg1)
-- True
--
-- Map from foreign-key strings to integers.
--
-- >>> type Eg3 = Ref (Prim String % Ø) 'Here % Ø ↦ Prim Int % Ø
-- >>> eg3 = [(("three":%Ø_):%Ø_, 3:%Ø_), (("two":%Ø_):%Ø_, 2:%Ø_), (("one":%Ø_):%Ø_, 1:%Ø_)]
-- >>> convertTo @Eg3 @_ @[Tuple] Proxy eg3
-- [(83...7c,[Group [Atom "ethree"],Atom "\ETX"]),(7f...ee,[Group [Atom "ctwo"],Atom "\STX"]),(be...20,[Group [Atom "cone"],Atom "\SOH"])]
-- >>> Right eg3 == convertFrom @Eg3 @_ @[Tuple] Proxy (convertTo @Eg3 @_ @[Tuple] Proxy eg3)
-- True
instance (Convert pk pk' Row, Convert v v' Row, RowWidth pk, Ord pk')
      => Convert (pk ↦ v) (Assoc pk' v') [Tuple] where
    convertTo _
        = map (\(pk,v) -> (hashRow pk, pk <> v))
        . map (bimap
            (convertTo @pk Proxy)
            (convertTo @v Proxy))
    convertFrom _
        --  Convert both elements of each pair
        =   mapM (\(pk,v) -> liftA2 (,)
                (convertFrom @pk Proxy pk)
                (convertFrom @v Proxy v))
        --  Take the pk from the row, verify the hash, return the key & value
        <=< mapM (\(hash,row) ->
                let (pk,v) = splitAt (rowWidth @pk Proxy) row in
                if hash == hashRow pk
                then pure (pk,v)
                else Left WrongPkHash{gotHash=hash, expectedHash=hashRow pk})

instance (Convert c c' Col, Convert cs cs' Row)
      => Convert (c % cs) (c' :% cs') Row where
    convertTo _ (c :% cs) = convertTo @c  Proxy c
                          : convertTo @cs Proxy cs
    convertFrom _ []     = Left TooFewColumns
    convertFrom _ (x:xs) = liftA2 (:%) (convertFrom @c  Proxy x)
                                       (convertFrom @cs Proxy xs)

instance Convert Ø Ø_ Row where
    convertTo   _ Ø_    = []
    convertFrom _ []    = pure Ø_
    convertFrom _ (_:_) = Left TooManyColumns

instance (Serialise a) => Convert (Prim a) a Col where
    convertFrom _ (Atom  x) = bimap DeserialiseFailure id $ deserialiseOrFail x
    convertFrom _ (Group _) = Left GotGroup'ExpectedAtom
    convertTo   _           = Atom . serialise

instance (Convert fk fk' Row) => Convert (Ref fk index) fk' Col where
    convertFrom _ (Atom  _) = Left GotAtom'ExpectedGroup
    convertFrom _ (Group x) = convertFrom @fk Proxy x
    convertTo   _           = Group . convertTo @fk Proxy
