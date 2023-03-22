-- {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, DataKinds #-} -- TypeFamilies implies KindSignatures,ExplictNamespaces
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-} -- implies TypeSynonymInstances
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE LambdaCase #-}
-- FIXME: undecidable-instances here lets us convert to our hetlist.
{-# LANGUAGE UndecidableInstances #-}
module Mog.UI where

import Data.Bifunctor (bimap)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (type Nat, type Symbol)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Mog.Index (RefIndex)
import Mog.Tuple (IsTuple, ToTuple(TupleOf), TupleList, ToTupleList(..))
import qualified Mog.Schema as S
import Mog.Instance

-- $setup
-- >>> import Mog (MRDT(α, γ, Abstracted))
-- >>> import Tutorial




-- * UI Schema datatypes

infix  5 ::::
infixr 6 :>
infixr 7 :@

-- | A named datatype with a tuple of relations.
newtype Dt (name :: Symbol) rel_tuple = Dt rel_tuple
    deriving (Eq, Ord, Show)

-- | A named relation with its tuple type
newtype (name :: Symbol) :::: pk_v = Rel pk_v
    deriving (Eq, Ord, Show)

-- | A primary key in a relation tuple.
type pk :> v = Map pk v

-- | A foreign key in a relation tuple.
newtype a :@ (index :: Nat) = Ref a
    deriving (Eq, Ord, Show)
-- TODO: rename to @ on a newer GHC version
--
-- TODO: come up with a way of saying "Fk 1" instead of "a :@ 1" because it
-- would be nice to not need to repeat the type.

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 ::  "mapping" ::::   k:@1  :> v
            :~: "mapping" :::: ((k:@1) :> v)
_testPrec10 = Refl




-- * Conversion to schema

type family ToSchema a :: Type where
    ToSchema (Dt name rels) = S.Schema name (ToSchemaRelsTL rels)

type ToSchemaRelsTL x = ToSchemaRels (TupleList (TupleOf (IsTuple x) x)) -- UndecidableInstances (nested tyfams)

type family ToSchemaRels a :: Type where
    ToSchemaRels (x,xs) = ToSchemaRel x S.& ToSchemaRels xs
    ToSchemaRels ()     = S.TablesEnd

type family ToSchemaRel a :: Type where
    ToSchemaRel (name :::: pk_v) = S.Table name (ToSchemaRel pk_v)
    ToSchemaRel (Set k)  = ToSchemaRel (k :> ()) -- ToSchemaColsTL k S.↦ ToSchemaColsTL ()
    ToSchemaRel (k :> v) = ToSchemaColsTL k S.↦ ToSchemaColsTL v

type ToSchemaColsTL x = ToSchemaCols (TupleList (TupleOf (IsTuple x) x)) -- UndecidableInstances (nested tyfams)

type family ToSchemaCols a :: Type where
    ToSchemaCols (x,xs) = ToSchemaCol (IsRef x) x S.% ToSchemaCols xs
    ToSchemaCols ()     = S.Ø

type family IsRef a :: Bool where
    IsRef (a :@ ix) = 'True
    IsRef a = 'False

type family ToSchemaCol (b::Bool) a :: Type where
    ToSchemaCol 'True (a :@ ix) = S.Ref (ToSchemaColsTL a) (RefIndex ix)
    ToSchemaCol 'False a        = S.Prim a

-- * Conversion to inst

-- | A named datatype with a tuple of relations
--
-- >>> let om = omInsert "three" 3 . omInsert "two" 2 . omInsert "one" 1 $ omEmpty :: OrderedMap String Int
-- >>> α om == dtUser @(Abstracted (OrderedMap String Int)) (dtInst (α om))
-- True
class DtC u where
    dtInst :: u -> Inst (ToSchema u)
    dtUser :: Inst (ToSchema u) -> u
instance
        ( ToTupleList b rels -- UndecidableInstances (variable not in head)
        , RelsC (TupleList (TupleOf b rels)) -- UndecidableInstances (nesting)
        ) =>
    DtC (Dt name rels) where
    dtInst (Dt x) = relsInst . toTL $ x
    dtUser = Dt . fromTL . relsUser


class RelsC u where
    relsInst :: u -> Inst (ToSchemaRels u)
    relsUser :: Inst (ToSchemaRels u) -> u
instance RelsC () where
    relsInst () = TTablesEnd
    relsUser TTablesEnd = ()
instance (RelC x, RelsC xs) => RelsC (x,xs) where
    relsInst (x,xs) = relInst x :& relsInst xs
    relsUser (x:&xs) = (relUser x, relsUser xs)


-- | A set of tuples that form a relation.
--
-- Set of pairs.
--
-- >>> eg1 = Set.fromList [("a", "bc"), ("ab", "c")]
-- >>> relInst eg1
-- [("a" :% ("bc" :% Ø_),Ø_),("ab" :% ("c" :% Ø_),Ø_)]
-- >>> eg1 == relUser (relInst eg1)
-- True
class RelC u where
    relInst :: u -> Inst (ToSchemaRel u)
    relUser :: Inst (ToSchemaRel u) -> u
instance (RelC pk_v) => RelC (name :::: pk_v) where
    relInst (Rel x) = relInst x
    relUser = Rel . relUser
instance
        ( Ord pk
        , ToTupleList b pk -- UndecidableInstances (variable not in head)
        , ColsC (TupleList (TupleOf b pk)) -- UndecidableInstances (nesting)
        ) =>
    RelC (Set pk) where
    -- XXX could go via map, but would reconstruct the tree structure
    --relInst = relInst . Map.fromSet (const ())
    --relUser = Map.keysSet @_ @() . relUser
    relInst
        = map (\pk -> (pk,Ø_))
        . map colsInst
        . map toTL
        . Set.toList
    relUser
        = Set.fromList
        . map fromTL
        . map colsUser
        . map (\(pk,Ø_) -> pk)
-- | A map (a set of tuples distinguished by a primary-key) that form a relation.
--
-- >>> eg0 = Map.fromList [(Ref "three",3), (Ref "two",2), (Ref "one",1)] :: String:@1 :> Int
-- >>> relInst eg0
-- [(("one" :% Ø_) :% Ø_,1 :% Ø_),(("three" :% Ø_) :% Ø_,3 :% Ø_),(("two" :% Ø_) :% Ø_,2 :% Ø_)]
-- >>> eg0 == relUser (relInst eg0)
-- True
instance
        ( Ord pk
        , ToTupleList b₁ pk -- UndecidableInstances (variable not in head)
        , ToTupleList b₂ v  -- UndecidableInstances (variable not in head)
        , ColsC (TupleList (TupleOf b₁ pk)) -- UndecidableInstances (nesting)
        , ColsC (TupleList (TupleOf b₂ v))  -- UndecidableInstances (nesting)
        ) =>
    RelC (pk :> v) where
    relInst
        = map (bimap colsInst colsInst)
        . map (bimap toTL toTL)
        . Map.toList
    relUser
        = Map.fromList
        . map (bimap fromTL fromTL)
        . map (bimap colsUser colsUser)


class ColsC u where
    colsInst :: u -> Inst (ToSchemaCols u)
    colsUser :: Inst (ToSchemaCols u) -> u
instance ColsC () where
    colsInst () = Ø_
    colsUser Ø_ = ()
instance (ColC b x, ColsC xs) => ColsC (x,xs) where
    colsInst (x,xs) = colInst x :% colsInst xs
    colsUser (x:%xs) = (colUser x, colsUser xs)


class (b ~ IsRef u) => ColC b u where
    colInst :: u -> Inst (ToSchemaCol b u)
    colUser :: Inst (ToSchemaCol b u) -> u
instance -- OVERLAPPABLE
        ( IsRef u ~ 'False
        ) =>
    ColC 'False u where
    colInst = id
    colUser = id
instance -- {-# OVERLAPS #-}
        ( ToTupleList b a
        , ColsC (TupleList (TupleOf b a))
        ) =>
    ColC 'True (a :@ ix) where
    colInst (Ref x) = colsInst $ toTL x
    colUser = Ref . fromTL . colsUser



