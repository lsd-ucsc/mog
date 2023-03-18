{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, DataKinds #-} -- TypeFamilies implies KindSignatures,ExplictNamespaces
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-} -- implies TypeSynonymInstances
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
-- NOTE: if rolling-back undecidableinstances and tuple-lists, see
-- commit:da08ef2 for RelsC and RelC instances that treat a tuple as a "list"
-- of relations and how they handle the "singleton list" case by duplicating
-- the instance which processes a single element of the "list."
{-# LANGUAGE UndecidableInstances #-}
module Mog.UI where

import Codec.Serialise (Serialise, serialise, deserialiseOrFail, DeserialiseFailure)
import Control.Applicative (liftA2)
import Control.Monad ((<=<))
import Crypto.Hash (hashlazy, Digest, SHA1)
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (type Nat, type Symbol, KnownSymbol, symbolVal, TypeError, type ErrorMessage(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Mog.Output (Row, Col(..))
import Mog.Tuple

-- CONTINUE
-- 0) test each of the storage classes
-- 1) validation instances
-- 2) prune function
-- 3) MRDTs class w/examples




-- * Inductive type-nats

data Index = Here | There Index

-- | Exists for documentation.
type family NatIndex (a :: Nat) :: Index where
    NatIndex 0 =                                                                         'Here
    NatIndex 1 =                                                                 ('There 'Here)
    NatIndex 2 =                                                         ('There ('There 'Here))
    NatIndex 3 =                                                 ('There ('There ('There 'Here)))
    NatIndex 4 =                                         ('There ('There ('There ('There 'Here))))
    NatIndex 5 =                                 ('There ('There ('There ('There ('There 'Here)))))
    NatIndex 6 =                         ('There ('There ('There ('There ('There ('There 'Here))))))
    NatIndex 7 =                 ('There ('There ('There ('There ('There ('There ('There 'Here)))))))
    NatIndex 8 =         ('There ('There ('There ('There ('There ('There ('There ('There 'Here))))))))
    NatIndex 9 = ('There ('There ('There ('There ('There ('There ('There ('There ('There 'Here)))))))))
    NatIndex _ = TypeError ('Text "too many") -- UndecidableInstances

-- | @RefIndex n ≡ NatIndex (n - 1)@ so that you can say @1@ to mean the next
-- relation, and @2@ to mean /two/ relations over. Accordingly @0@ isn't here
-- because you can't have a fk to yourself.
type family RefIndex (a :: Nat) :: Index where
    RefIndex 1 =                                                                 'Here
    RefIndex 2 =                                                         ('There 'Here)
    RefIndex 3 =                                                 ('There ('There 'Here))
    RefIndex 4 =                                         ('There ('There ('There 'Here)))
    RefIndex 5 =                                 ('There ('There ('There ('There 'Here))))
    RefIndex 6 =                         ('There ('There ('There ('There ('There 'Here)))))
    RefIndex 7 =                 ('There ('There ('There ('There ('There ('There 'Here))))))
    RefIndex 8 =         ('There ('There ('There ('There ('There ('There ('There 'Here)))))))
    RefIndex 9 = ('There ('There ('There ('There ('There ('There ('There ('There 'Here))))))))
    RefIndex _ = TypeError ('Text "too many") -- UndecidableInstances




-- * UI Schema datatypes

infix  5 ∷
infixr 6 ↦
infixr 7 :@

-- | A named datatype with a tuple of relations.
newtype (name :: Symbol) ::: rel_tuple = Dt rel_tuple

-- | A named relation with its tuple type
newtype (name :: Symbol) ∷ pk_v = Rel pk_v

-- | A primary key in a relation tuple.
type pk ↦ v = Map pk v

-- | A foreign key in a relation tuple.
newtype a :@ (index :: Nat) = Ref a
-- TODO: rename to @ on a newer GHC version

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 ::  "mapping" ∷   k:@1  ↦ v
            :~: "mapping" ∷ ((k:@1) ↦ v)
_testPrec10 = Refl
-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec20 ::
        "ordered-map" :::   ( "order" ∷ Set (k:@2, k:@2)
                            , "mapping" ∷ k:@1 ↦ v
                            , "keys" ∷ Set k)
    :~: "ordered-map" :::   ( "order" ∷ Set (k:@2, k:@2)
                            , "mapping" ∷ ((k:@1) ↦ v)
                            , "keys" ∷ Set k)
_testPrec20 = Refl
-- TODO: can delete this test once we have more examples

-- TODO: complete this, and probably define it elsewhere
class {-Relations (Abstracted a) =>-} MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a




-- * Validation

--VALIDATION--class Pks a where
--VALIDATION--    type PksFor a :: Type
--VALIDATION--    getPks :: a -> PksFor a
--VALIDATION--
--VALIDATION--instance Pks () where
--VALIDATION--    type PksFor () = ()
--VALIDATION--    getPks () = ()
--VALIDATION--instance (Pks x, Pks xs) => Pks (x, xs) where
--VALIDATION--    type PksFor (x, xs) = (PksFor x, PksFor xs)
--VALIDATION--    getPks (x, xs) = (getPks x, getPks xs)
--VALIDATION--
--VALIDATION--instance Pks pk_v => Pks (name ∷ pk_v) where
--VALIDATION--    type PksFor (name ∷ pk_v) = PksFor pk_v
--VALIDATION--    getPks (Rel x) = getPks x
--VALIDATION--instance Pks (pk ↦ v) where
--VALIDATION--    type PksFor (pk ↦ v) = Set pk
--VALIDATION--    getPks = Map.keysSet
--VALIDATION--instance Pks (Set pk) where
--VALIDATION--    type PksFor (Set pk) = Set pk
--VALIDATION--    getPks = id
--VALIDATION--
--VALIDATION---- class Fk (ix :: Index) k rs where
--VALIDATION----     aa :: PksFor 




-- * Storage

data RestoreError
    = WrongDatatypeName {got::String, expected::String}
    | WrongRelationName {got::String, expected::String}
    | TooFewRelations
    | TooManyRelations
    | TooFewElements
    | TooManyElements
    | WrongPkHash {gotHash::Digest SHA1, expectedHash::Digest SHA1}
    | GotGroup'ExpectedAtom
    | GotAtom'ExpectedGroup
    | DeserialiseFailure DeserialiseFailure

type Option = Either RestoreError

-- ** Storage datatypes

type Datatype = (String, [Relation])
type Relation = (String, [Tuple])
type Tuple = (Digest SHA1, Row)

-- ** Storage classes

class Dt a where
    storeDt :: a -> Datatype
    loadDt :: Datatype -> Option a
class Rels a where
    storeRels :: a -> [Relation]
    loadRels :: [Relation] -> Option a

class RelC a where
    storeRel :: a -> Relation
    loadRel :: Relation -> Option a
class Tups a where
    storeTups :: a -> [Tuple]
    loadTups :: [Tuple] -> Option a

class Elts a where
    storeElts :: a -> Row
    loadElts :: Row -> Option a
class Elt a where
    storeElt :: a -> Col
    loadElt :: Col -> Option a

-- ** Storage instances

-- | A named datatype with a tuple of relations
instance
    -- XXX: might need this as a class/instance when used in the context of a DB
    -- with multiple datatypes
        ( KnownSymbol name
        , ToTupleList b rels -- UndecidableInstances (variable not in head)
        , Rels (TupleList (TupleOf b rels)) -- UndecidableInstances (nesting)
        ) =>
    Dt (name ::: rels) where
    -- TODO: pre-process all relations in a single pass to prune out bad FKs. To do
    -- this, reuse the "validation" classes in Mog.Inst that check (1) fk validity
    -- and (2) implement prune
    storeDt (Dt x) =
        ( symbolVal @name Proxy
        , storeRels $ toTL x
        )
    -- TODO: post-process loaded data to prune out bad FKs
    loadDt (n, x)
        | n == symbolVal @name Proxy = Dt . fromTL <$> loadRels x
        | otherwise = Left WrongDatatypeName{got=n, expected=symbolVal @name Proxy}

-- | A tuple-list of relations
instance
    Rels () where
    storeRels () = []
    loadRels ( []) = pure ()
    loadRels (_:_) = Left TooManyRelations
instance
        ( RelC x
        , Rels xs
        ) =>
    Rels (x, xs) where
    storeRels (x,xs) = storeRel x : storeRels xs
    loadRels (  []) = Left TooFewRelations
    loadRels (x:xs) = liftA2 (,) (loadRel x) (loadRels xs)

-- | A named relation with a set or map of tuples
instance
    -- XXX: could be pulled out to plain old functions if Rels-instance were
    -- hardcoded to `name∷pk_v`
        ( KnownSymbol name
        , Tups pk_v
        ) =>
    RelC (name ∷ pk_v) where
    storeRel (Rel x) =
        (symbolVal @name Proxy, storeTups x)
    loadRel (n, x)
        | n == symbolVal @name Proxy = Rel <$> loadTups x
        | otherwise = Left WrongRelationName{got=n, expected=symbolVal @name Proxy}

-- | A set of tuples that form a relation
instance
        ( Ord pk
        , ToTupleList b pk                -- UndecidableInstances
        , Elts (TupleList (TupleOf b pk)) -- UndecidableInstances
        ) =>
    Tups (Set pk) where
    storeTups
        = map (\pk -> (hashRow pk, pk))
        . map storeElts
        . map toTL
        . Set.toList
        -- XXX: opportunity to apply Fk-tuple-filter here using Set.intersection if the approach described in storeDt doesn't work
    loadTups
        -- XXX: opportunity to apply Fk-tuple-filter here using Set.intersection if the approach described in storeDt doesn't work
        =   fmap Set.fromList
        .   fmap (map fromTL)
        .   mapM loadElts
        <=< mapM (\(hash,row) ->
                if hash == hashRow row then pure row else Left WrongPkHash{gotHash=hash, expectedHash=hashRow row})
-- | A map (a set of tuples distinguished by a primary-key) that form a relation
instance
        ( Ord pk
        , ToTupleList b₁ pk -- UndecidableInstances (variable not in head)
        , ToTupleList b₂ v  -- UndecidableInstances (variable not in head)
        , pkTL ~ TupleList (TupleOf b₁ pk)
        , vTL  ~ TupleList (TupleOf b₂ v)
        , TupleListSize pkTL -- UndecidableInstances (nesting)
        , Elts pkTL          -- UndecidableInstances (nesting)
        , Elts vTL           -- UndecidableInstances (nesting)
        ) =>
    Tups (pk ↦ v) where
    storeTups
        = map (\(pk,v) -> (hashRow pk, pk <> v))
        . map (bimap storeElts storeElts)
        . map (bimap toTL toTL)
        . Map.toList
        -- XXX: opportunity to apply Fk-tuple-filter here using Map.restrictKeys if the approach described in storeDt doesn't work
    loadTups
        -- XXX: opportunity to apply Fk-tuple-filter here using Map.restrictKeys if the approach described in storeDt doesn't work
        =   fmap Map.fromList
        .   fmap (map $ bimap fromTL fromTL)
        .   mapM (\(pk,v) -> liftA2 (,) (loadElts pk) (loadElts v))
        <=< mapM (\(hash,row) ->
                let (pk,v) = splitAt (sizeTLp @pkTL Proxy) row in
                if hash == hashRow pk then pure (pk,v) else Left WrongPkHash{gotHash=hash, expectedHash=hashRow row})

-- | A tuple-list of elements
instance
    Elts () where
    storeElts () = []
    loadElts ( []) = pure ()
    loadElts (_:_) = Left TooManyElements
instance
        ( Elt x
        , Elts xs
        ) =>
    Elts (x, xs) where
    storeElts (x,xs) = (storeElt x) : storeElts xs
    loadElts (  []) = Left TooFewElements
    loadElts (x:xs) = liftA2 (,) (loadElt x) (loadElts xs)

-- A primitive element
instance Serialise a => Elt a where
    storeElt = Atom . serialise
    loadElt (Atom bs )   = bimap DeserialiseFailure id $ deserialiseOrFail bs
    loadElt (Group _row) = Left GotGroup'ExpectedAtom
-- A foreign-key reference element
instance
        ( ToTupleList b a                -- UndecidableInstances
        , Elts (TupleList (TupleOf b a)) -- UndecidableInstances
        ) =>
    Elt (a :@ ix) where
    storeElt (Ref x) = Group . storeElts . toTL $ x
    loadElt (Atom _bs)  = Left GotAtom'ExpectedGroup
    loadElt (Group row) = Ref . fromTL <$> loadElts row

-- ** Storage utilities

hashRow :: Row -> Digest SHA1
hashRow = hashlazy . hashInputRow

hashInputRow :: Row -> ByteString
hashInputRow = mconcat . fmap hashChunksCol

hashChunksCol :: Col -> ByteString
hashChunksCol (Atom bs) = bs
hashChunksCol (Group row) = hashInputRow row
