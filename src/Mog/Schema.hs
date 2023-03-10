{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Mog.Schema (
  type Schema,
  type (&),
  type TablesEnd,

  type Table,
  type (↦),

  type (%),
  type Ø,

  type Ref,
  type Prim,

  Index(..),
) where

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (Symbol)

infixr 5 &
infix  6 ↦
infixr 7 %

data Schema (name :: Symbol) tables -- ^ Table-group wrapper
data t & ts -- ^ Table-group cons
data TablesEnd -- ^ Table-group nil

data Table (name :: Symbol) pk_v -- ^ Table (one) wrapper
data pk ↦ v -- ^ Table (one) spec

data c % cs -- ^ Tuple cons
data Ø -- ^ Tuple nil

data Index = Here | There Index
data Ref fk (index :: Index)
data Prim a

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 :: Int & Char % Word % () ↦ String % Float % () & Double
           :~: Int & (((Char % (Word % ())) ↦ (String % (Float % ()))) & Double)
_testPrec10 = Refl
