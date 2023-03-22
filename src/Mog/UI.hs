-- {-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, DataKinds #-} -- TypeFamilies implies KindSignatures,ExplictNamespaces
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleInstances #-} -- implies TypeSynonymInstances
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE LambdaCase #-}
-- FIXME: undecidable-instances here lets us convert to our hetlist.
{-# LANGUAGE UndecidableInstances #-}
module Mog.UI where

import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (type Nat, type Symbol, KnownSymbol, symbolVal, TypeError, type ErrorMessage(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Mog.Index (RefIndex)
import Mog.Tuple (IsTuple, ToTuple(TupleOf), TupleList)
import Mog.Tuple.HList (TupleToRelList(..))
import qualified Mog.Schema as S




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
    ToSchema (Dt name rels) = S.Schema name (ToSchemaRels rels)

type ToSchemaRels x = ToSchemaRels_ (TupleList (TupleOf (IsTuple x) x)) -- UndecidableInstances (nested tyfams)

type family ToSchemaRels_ a :: Type where
    ToSchemaRels_ (x,xs) = ToSchemaRel x S.& ToSchemaRels_ xs
    ToSchemaRels_ ()     = S.TablesEnd

type family ToSchemaRel a :: Type where
    ToSchemaRel (name :::: Set k ) = S.Table name (ToSchemaCols k S.↦ ToSchemaCols ())
    ToSchemaRel (name :::: k :> v) = S.Table name (ToSchemaCols k S.↦ ToSchemaCols v)

type ToSchemaCols x = ToSchemaCols_ (TupleList (TupleOf (IsTuple x) x)) -- UndecidableInstances (nested tyfams)

type family ToSchemaCols_ a :: Type where
    ToSchemaCols_ (x,xs) = ToSchemaCol x S.% ToSchemaCols_ xs
    ToSchemaCols_ ()     = S.Ø

type family ToSchemaCol a :: Type where
    ToSchemaCol (a :@ ix) = S.Ref (ToSchemaCols a) (RefIndex ix)
    ToSchemaCol a         = S.Prim a
