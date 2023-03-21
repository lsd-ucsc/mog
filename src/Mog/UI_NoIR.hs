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
module Mog.UI_NoIR where

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
-- 1) validation instances
-- 2) prune function
-- 3) MRDTs class w/examples
-- 4) MERGE-TYPES AND MERGE-FUNCTIONS

-- $setup
-- >>> :set -XTypeOperators
-- >>> :set -XDataKinds




-- * UI Schema datatypes

infix  5 ∷
infixr 6 ↦
infixr 7 :@

-- | A named datatype with a tuple of relations.
newtype (name :: Symbol) ::: rel_tuple = Dt rel_tuple
    deriving (Eq, Ord, Show)

-- | A named relation with its tuple type
newtype (name :: Symbol) ∷ pk_v = Rel pk_v
    deriving (Eq, Ord, Show)

-- | A primary key in a relation tuple.
type pk ↦ v = Map pk v

-- | A foreign key in a relation tuple.
newtype a :@ (index :: Nat) = Ref a
    deriving (Eq, Ord, Show)
-- TODO: rename to @ on a newer GHC version
--
-- TODO: come up with a way of saying "Fk 1" instead of "a :@ 1" because it
-- would be nice to not need to repeat the type.

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 ::  "mapping" ∷   k:@1  ↦ v
            :~: "mapping" ∷ ((k:@1) ↦ v)
_testPrec10 = Refl

-- TODO: complete this, and probably define it elsewhere
class {-Relations (Abstracted a) =>-} MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a




-- * Validation

-- (1) traverse to validate FK types
-- (2) traverse to obtain PK sets
-- (3) traverse to prune
class ValidDt a where
    pruneDt :: a -> a
instance
        ( ToTupleList b rels -- UndecidableInstances (variable not in head)
        , ValidRels (TupleList (TupleOf b rels)) -- UndecidableInstances (nesting)
        ) =>
    ValidDt (name ::: rels) where
    pruneDt (Dt x) = Dt . fromTL . pruneRels . toTL $ x

class ValidRels a where
    type RelsPks a :: Type
    relsPks :: a -> RelsPks a
    pruneRels :: a -> a
instance
    ValidRels () where
    type RelsPks () = ()
    relsPks () = ()
    pruneRels () = ()
instance
    ValidRels (x, xs) where
    -- | Prune referenced tables first. Consider this
    -- example:
    --
    -- ( "foo" has {a:@1:@1, b:@1:@1, c:@1:@1}
    -- , "bar" has {a:@1,    b:@1,    c:@1   }
    -- , "baz" has {a,       b               }
    -- )
    --
    -- Here we should prune "bar" of c@1 first by observing
    -- "baz", so that "foo" can learn that c@1@1 is absent.
--  type RelsPks (x, xs) = (RelPks x, PksFor xs)
--  pruneRels (x, xs) =
--      let (xs', pks) = pruneRels xs in
--      undefined --- (

class ValidRel a as where
    type RelPks a as :: Type
    relPks :: Proxy as -> a -> RelPks a as
    pruneRel :: Proxy as -> RelPks a as -> a -> a
--instance ValidRel (name ∷ a) as where
--    type RelPks (name ∷ a) as = TupsPks a as
--    relPks _ (Rel x) = tupsPks @as Proxy x

class ValidTups a as where
    type TupsPks a as :: Type
    tupsPks :: Proxy as -> a -> TupsPks a as
    pruneTups :: Proxy as -> TupsPks a as -> a -> a
--instance ValidTups (Set a) as 

-- class Pks a where
--     type PksFor a :: Type
--     getPks :: a -> PksFor a
-- instance
--         ( ToTupleList b rels -- UndecidableInstances (variable not in head)
--         , ValidRels (TupleList (TupleOf b rels)) -- UndecidableInstances (nesting)
--         ) =>
--     ValidDt (name ::: rels) where
--     pruneDt (Dt x) = Dt . fromTL . fst . pruneRels . toTL $ x
-- 
-- --instance Pks () where
-- --    type PksFor () = ()
-- --    getPks () = ()
-- --instance (Pks x, Pks xs) => Pks (x, xs) where
-- --    type PksFor (x, xs) = (PksFor x, PksFor xs)
-- --    getPks (x, xs) = (getPks x, getPks xs)
-- --
-- --instance Pks pk_v => Pks (name ∷ pk_v) where
-- --    type PksFor (name ∷ pk_v) = PksFor pk_v
-- --    getPks (Rel x) = getPks x
-- --instance Pks (pk ↦ v) where
-- --    type PksFor (pk ↦ v) = Set pk
-- --    getPks = Map.keysSet
-- --instance Pks (Set pk) where
-- --    type PksFor (Set pk) = Set pk
-- --    getPks = id
-- 
-- -- class Fk (ix :: Index) k rs where
-- --     aa :: PksFor 





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
    deriving Eq

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
--
-- >>> :{
-- type OrderedMap k v
--  = "ordered-map"
--  ::: ( "order" ∷ Set (k:@2, k:@2)
--      , "mapping" ∷ k:@1 ↦ v
--      , "keys" ∷ Set k )
-- omExample :: OrderedMap String Int
-- omExample =
--  Dt  ( Rel $ Set.fromList [(Ref "one", Ref "two"), (Ref "two", Ref "three")]
--      , Rel $ Map.fromList [(Ref "one",1), (Ref "two",2), (Ref "three",3)]
--      , Rel $ Set.fromList ["one", "two", "three"] )
-- :}
--
-- >>> Right omExample == loadDt (storeDt omExample)
-- True
--
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

-- | A set of tuples that form a relation.
--
-- Set of singleton tuples.
--
-- >>> eg0 = Set.fromList ["hello", "world"]
-- >>> storeTups eg0
-- [(34...af,[Atom "ehello"]),(70...e2,[Atom "eworld"])]
-- >>> Right eg0 == loadTups (storeTups eg0)
-- True
--
-- Set of pairs.
--
-- >>> eg1 = Set.fromList [("a", "bc"), ("ab", "c")]
-- >>> storeTups eg1
-- [(fb...d0,[Atom "aa",Atom "bbc"]),(5c...db,[Atom "bab",Atom "ac"])]
-- >>> Right eg1 == loadTups (storeTups eg1)
-- True
--
-- Hyperedges in a hypergraph (the other two relations for edges and nodes
-- aren't shown).
--
-- >>> eg2 = Set.fromList [(Ref (Ref 2, Ref 5), Ref (Ref 3, Ref 9))] :: Set ((Int:@2, Int:@2):@1, (Int:@2, Int:@2):@1)
-- >>> storeTups eg2
-- [(44...d6,[Group [Group [Atom "\STX"],Group [Atom "\ENQ"]],Group [Group [Atom "\ETX"],Group [Atom "\t"]]])]
-- >>> Right eg2 == loadTups (storeTups eg2)
-- True
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
        -- FIXME: merge duplicate tuples here with: M.keysSet . uncurry M.fromListWith
        .   fmap (map fromTL)
        .   mapM loadElts
        <=< mapM (\(hash,row) ->
                if hash == hashRow row then pure row else Left WrongPkHash{gotHash=hash, expectedHash=hashRow row})
-- | A map (a set of tuples distinguished by a primary-key) that form a relation.
--
-- >>> eg0 = Map.fromList [(Ref "three",3), (Ref "two",2), (Ref "one",1)] :: String:@1 ↦ Int
-- >>> storeTups eg0
-- [(be...20,[Group [Atom "cone"],Atom "\SOH"]),(83...7c,[Group [Atom "ethree"],Atom "\ETX"]),(7f...ee,[Group [Atom "ctwo"],Atom "\STX"])]
-- >>> Right eg0 == loadTups (storeTups eg0)
-- True
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
        -- FIXME: merge duplicate tuples here with: M.fromListWith
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
instance -- {-# OVERLAPPABLE #-}
        Serialise a =>
    Elt a where
    storeElt = Atom . serialise
    loadElt (Atom bs )   = bimap DeserialiseFailure id $ deserialiseOrFail bs
    loadElt (Group _row) = Left GotGroup'ExpectedAtom
-- A foreign-key reference element
instance {-# OVERLAPS #-}
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
