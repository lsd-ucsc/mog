{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module for the datatype to which a schema instance is encoded or decoded
-- on the way to git
module Mog.Output where

import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Data.Text as Text (pack)

import Codec.Serialise (Serialise, serialise, deserialiseOrFail, DeserialiseFailure)
import Crypto.Hash (hashlazy, Digest, SHA1)

import Mog.Schema
import Mog.Instance

-- $setup
-- >>> :set -XTypeOperators
-- >>> :set -XDataKinds
-- >>> import Mog.Example
-- >>> import Mog.Index




-- * Storage format

-- | A datatype represented as a group of named characteristic relations.
type Datatype = (Text, [Relation])

-- | A characteristic relation represented as a group of db-tuples.
type Relation = (Text, [Tuple])

-- | A tuple containing the hash of PK columns and CBOR of all the columns.
type Tuple = (Digest SHA1, Row)

-- | CBOR encoded columns representing a db-tuple.
type Row = [Col]

-- | A column is either a single atom or group of columns.
data Col
    = Atom  ByteString Tag
    | Group Row
    deriving Show

-- | A short string tag indicating the role of a column in a tuple. It is used
-- as a file extension to control how git handles merge conflicts. Atoms with
-- 'pkTag' must not have conflicts (this is what the content-addressing of
-- hashing those values into the file path accomplishes). Atoms with any other
-- tag will be merged by the Mog merge-driver. Tags are ignored on load.
type Tag = Text

-- | Primary key.
pkTag :: Tag
pkTag = "pk"

-- | Value (not a primary key).
valTag :: Tag
valTag = "val"




-- * Instance of IR to storage format

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
    named :: Proxy a -> Text
instance (KnownSymbol name) => Named (Schema name ts) where
    named _ = Text.pack $ symbolVal @name Proxy
instance (KnownSymbol name) => Named (Table name pk_v) where
    named _ = Text.pack $ symbolVal @name Proxy

hashRow :: Row -> Digest SHA1
hashRow = hashlazy . hashInputRow

hashInputRow :: Row -> ByteString
hashInputRow = mconcat . fmap hashChunksCol

hashChunksCol :: Col -> ByteString
hashChunksCol (Atom bs _) = bs
hashChunksCol (Group row) = hashInputRow row




-- ** Classes to implement conversion

class (Inst i ~ a) => Convert i a b where
    convertTo   :: Proxy i -> a -> b
    convertFrom :: Proxy i -> b -> Option a

data RestoreError
    = WrongDatatypeName {got::Text, expected::Text}
    | WrongRelationName {got::Text, expected::Text}
    | TooFewRelations {expected::Text}
    | TooManyRelations {unexpected::Text}
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
    convertTo ir x =
        ( named ir
        , convertTo @ts Proxy x )
    convertFrom ir (n, x)
        | n == named ir = convertFrom @ts Proxy x
        | otherwise = Left WrongDatatypeName{got=n, expected=named ir}

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
    convertTo ir x =
        ( named ir
        , convertTo @pk_v Proxy x )
    convertFrom ir (n, x)
        | n == named ir = convertFrom @pk_v Proxy x
        | otherwise = Left WrongRelationName{got=n, expected=named ir}

-- |
--
-- Set of singleton tuples.
--
-- >>> type Eg0 = Prim String % Ø ↦ Ø
-- >>> eg0 = [("hello":%Ø_, Ø_), ("world":%Ø_, Ø_)]
-- >>> convertTo @Eg0 @_ @[Tuple] Proxy eg0
-- [(34...af,[Atom "ehello" "pk"]),(70...e2,[Atom "eworld" "pk"])]
-- >>> Right eg0 == convertFrom @Eg0 @_ @[Tuple] Proxy (convertTo @Eg0 @_ @[Tuple] Proxy eg0)
-- True
--
-- Set of pairs.
--
-- >>> type Eg1 = Prim String % Prim String % Ø ↦ Ø
-- >>> eg1 = [("a":%"bc":%Ø_, Ø_), ("ab":%"c":%Ø_, Ø_)]
-- >>> convertTo @Eg1 @_ @[Tuple] Proxy eg1
-- [(fb...d0,[Atom "aa" "pk",Atom "bbc" "pk"]),(5c...db,[Atom "bab" "pk",Atom "ac" "pk"])]
-- >>> Right eg1 == convertFrom @Eg1 @_ @[Tuple] Proxy (convertTo @Eg1 @_ @[Tuple] Proxy eg1)
-- True
--
-- Map from foreign-key strings to integers.
--
-- >>> type Eg3 = Ref (Prim String % Ø) 'Here % Ø ↦ Prim Int % Ø
-- >>> eg3 = [(("three":%Ø_):%Ø_, 3:%Ø_), (("two":%Ø_):%Ø_, 2:%Ø_), (("one":%Ø_):%Ø_, 1:%Ø_)]
-- >>> convertTo @Eg3 @_ @[Tuple] Proxy eg3
-- [(83...7c,[Group [Atom "ethree" "pk"],Atom "\ETX" "val"]),(7f...ee,[Group [Atom "ctwo" "pk"],Atom "\STX" "val"]),(be...20,[Group [Atom "cone" "pk"],Atom "\SOH" "val"])]
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
                (fromRow @pk Proxy pk)
                (fromRow @v  Proxy v))
        --  Take the pk from the row, verify the hash, return the key & value
        <=< mapM (\(hash,row) ->
                let (pk,v) = splitAt (rowWidth @pk Proxy) row in
                if hash == hashRow pk
                then pure (pk,v)
                else Left WrongPkHash{gotHash=hash, expectedHash=hashRow pk})

-- *** Row conversion

class ConvertRow a where
    toRow   :: Proxy a -> Tag -> Inst a -> Row
    fromRow :: Proxy a        -> Row    -> Option (Inst a)

instance (ConvertCol c, ConvertRow cs) => ConvertRow (c % cs) where
    toRow   _ tag (c:%cs) = toCol @c  Proxy tag c
                          : toRow @cs Proxy tag cs
    fromRow _     []      = Left TooFewColumns
    fromRow _     (x:xs)  = liftA2 (:%) (fromCol @c  Proxy x)
                                        (fromRow @cs Proxy xs)

instance ConvertRow Ø where
    toRow   _ _ Ø_    = []
    fromRow _   []    = pure Ø_
    fromRow _   (_:_) = Left TooManyColumns

-- *** Col conversion

class ConvertCol a where
    toCol   :: Proxy a -> Tag -> Inst a -> Col
    fromCol :: Proxy a        -> Col    -> Option (Inst a)

instance Serialise a => ConvertCol (Prim a) where
    toCol   _  tag x          = Atom (serialise x) tag
    fromCol _      (Atom x _) = bimap DeserialiseFailure id $ deserialiseOrFail x
    fromCol _      (Group _)  = Left GotGroup'ExpectedAtom

-- | A foreign key is the primary key of a another relation, so this instance
-- hardcodes the primary key tag.
instance ConvertRow fk => ConvertCol (Ref fk index) where
    toCol   _ _ x          = Group $ toRow @fk Proxy pkTag x
    fromCol _   (Atom _ _) = Left GotAtom'ExpectedGroup
    fromCol _   (Group x)  = fromRow @fk Proxy x
