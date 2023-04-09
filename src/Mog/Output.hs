{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Module for converting a schema instance to or from the storage format used
-- with git.
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

-- | A named list of characteristic relations.
type Datatype = (Text, [Relation])

-- | A named list of tuples.
type Relation = (Text, [Tuple])

-- | A row with the hash of its PK fields.
type Tuple = (Digest SHA1, [Field])

-- | CBOR serialized data for a single atom or group of fields. A list of
-- fields is called a row in identifiers, and a row with the hash of its PK
-- fields is called a tuple.
data Field
    = Atom  ByteString Tag
    | Group [Field]
    deriving (Eq, Ord, Show)

-- | A short string tag indicating the role of a field in a tuple. It is used
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

hashRow :: [Field] -> Digest SHA1
hashRow = hashlazy . hashInputRow

hashInputRow :: [Field] -> ByteString
hashInputRow = mconcat . fmap hashChunksField

hashChunksField :: Field -> ByteString
hashChunksField (Atom bs _) = bs
hashChunksField (Group row) = hashInputRow row




-- ** Classes to implement conversion

class (Inst i ~ a) => Convert i a b where
    convertTo   :: Proxy i -> a -> b
    convertFrom :: Proxy i -> b -> Option a

data RestoreError
    = WrongDatatypeName {got::Text, expected::Text}
    | WrongRelationName {got::Text, expected::Text}
    | TooFewRelations {expected::Text}
    | TooManyRelations {unexpected::Text}
    | TooFewFields
    | TooManyFields
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
    toRow   :: Proxy a -> Tag -> Inst a  -> [Field]
    fromRow :: Proxy a        -> [Field] -> Option (Inst a)

instance (ConvertField c, ConvertRow cs) => ConvertRow (c % cs) where
    toRow   _ tag (c:%cs) = toField @c  Proxy tag c
                          : toRow   @cs Proxy tag cs
    fromRow _     []      = Left TooFewFields
    fromRow _     (x:xs)  = liftA2 (:%) (fromField @c  Proxy x)
                                        (fromRow   @cs Proxy xs)

instance ConvertRow Ø where
    toRow   _ _ Ø_    = []
    fromRow _   []    = pure Ø_
    fromRow _   (_:_) = Left TooManyFields

-- *** Field conversion

class ConvertField a where
    toField   :: Proxy a -> Tag -> Inst a -> Field
    fromField :: Proxy a        -> Field  -> Option (Inst a)

instance Serialise a => ConvertField (Prim a) where
    toField   _  tag x          = Atom (serialise x) tag
    fromField _      (Atom x _) = bimap DeserialiseFailure id $ deserialiseOrFail x
    fromField _      (Group _)  = Left GotGroup'ExpectedAtom

-- | A foreign key is the primary key of a another relation, so this instance
-- hardcodes the primary key tag.
instance ConvertRow fk => ConvertField (Ref fk index) where
    toField   _ _ x          = Group $ toRow @fk Proxy pkTag x
    fromField _   (Atom _ _) = Left GotAtom'ExpectedGroup
    fromField _   (Group x)  = fromRow @fk Proxy x
