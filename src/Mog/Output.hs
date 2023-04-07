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
-- >>> :set -XTypeOperators
-- >>> :set -XDataKinds
-- >>> import Mog.Example
-- >>> import Mog.Index




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
    = Atom  ByteString Tag
    | Group Row
    deriving Show

-- | A string tag indicating the role of a column in the tuple.
type Tag = String

-- | Primary key.
pkTag :: Tag
pkTag = "pk"

-- | Value (not a primary key).
valTag :: Tag
valTag = "val"




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
hashChunksCol (Atom bs _) = bs
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
    | WrongTag {got::Tag, expected::Tag}
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
-- [(34...af,[Atom "ehello" Pk]),(70...e2,[Atom "eworld" Pk])]
-- >>> Right eg0 == convertFrom @Eg0 @_ @[Tuple] Proxy (convertTo @Eg0 @_ @[Tuple] Proxy eg0)
-- True
--
-- Set of pairs.
--
-- >>> type Eg1 = Prim String % Prim String % Ø ↦ Ø
-- >>> eg1 = [("a":%"bc":%Ø_, Ø_), ("ab":%"c":%Ø_, Ø_)]
-- >>> convertTo @Eg1 @_ @[Tuple] Proxy eg1
-- [(fb...d0,[Atom "aa" Pk,Atom "bbc" Pk]),(5c...db,[Atom "bab" Pk,Atom "ac" Pk])]
-- >>> Right eg1 == convertFrom @Eg1 @_ @[Tuple] Proxy (convertTo @Eg1 @_ @[Tuple] Proxy eg1)
-- True
--
-- Map from foreign-key strings to integers.
--
-- >>> type Eg3 = Ref (Prim String % Ø) 'Here % Ø ↦ Prim Int % Ø
-- >>> eg3 = [(("three":%Ø_):%Ø_, 3:%Ø_), (("two":%Ø_):%Ø_, 2:%Ø_), (("one":%Ø_):%Ø_, 1:%Ø_)]
-- >>> convertTo @Eg3 @_ @[Tuple] Proxy eg3
-- [(83...7c,[Group [Atom "ethree" Pk],Atom "\ETX" Val]),(7f...ee,[Group [Atom "ctwo" Pk],Atom "\STX" Val]),(be...20,[Group [Atom "cone" Pk],Atom "\SOH" Val])]
-- >>> Right eg3 == convertFrom @Eg3 @_ @[Tuple] Proxy (convertTo @Eg3 @_ @[Tuple] Proxy eg3)
-- True
instance
        ( RowWidth pk
        , ConvertRow pk
        , ConvertRow v
        , Inst pk ~ pk'
        , Inst v  ~ v'
        , Ord pk'
        ) =>
    Convert (pk ↦ v) (Assoc pk' v') [Tuple] where
    convertTo _
        = map (\(pk,v) -> (hashRow pk, pk <> v))
        . map (bimap
            (toRow @pk Proxy pkTag)
            (toRow @v  Proxy valTag))
    convertFrom _
        --  Convert both elements of each pair
        =   mapM (\(pk,v) -> liftA2 (,)
                (fromRow @pk Proxy pkTag pk)
                (fromRow @v  Proxy valTag v))
        --  Take the pk from the row, verify the hash, return the key & value
        <=< mapM (\(hash,row) ->
                let (pk,v) = splitAt (rowWidth @pk Proxy) row in
                if hash == hashRow pk
                then pure (pk,v)
                else Left WrongPkHash{gotHash=hash, expectedHash=hashRow pk})


class ConvertRow a where
    toRow   :: Proxy a -> Tag -> Inst a -> Row
    fromRow :: Proxy a -> Tag -> Row    -> Option (Inst a)

instance (ConvertCol c, ConvertRow cs) => ConvertRow (c % cs) where
    toRow _ tag (c :% cs) = toCol @c  Proxy tag c
                          : toRow @cs Proxy tag cs
    fromRow _ _tag []     = Left TooFewColumns
    fromRow _  tag (x:xs) = liftA2 (:%) (fromCol @c  Proxy tag x)
                                        (fromRow @cs Proxy tag xs)

instance ConvertRow Ø where
    toRow   _ _r Ø_    = []
    fromRow _ _r []    = pure Ø_
    fromRow _ _r (_:_) = Left TooManyColumns


class ConvertCol a where
    toCol   :: Proxy a -> Tag -> Inst a -> Col
    fromCol :: Proxy a -> Tag -> Col    -> Option (Inst a)

instance Serialise a => ConvertCol (Prim a) where
    toCol   _  tag x                      = Atom (serialise x) tag
    fromCol _  tag (Atom x e) | tag == e  = bimap DeserialiseFailure id $ deserialiseOrFail x
                              | otherwise = Left WrongTag{got=e, expected=tag}
    fromCol _ _tag (Group _)              = Left GotGroup'ExpectedAtom

instance ConvertRow fk => ConvertCol (Ref fk index) where
    toCol   _ _tag            = Group . toRow @fk Proxy pkTag
    fromCol _ _tag (Atom _ _) = Left GotAtom'ExpectedGroup
    fromCol _ _tag (Group x)  = fromRow @fk Proxy pkTag x
