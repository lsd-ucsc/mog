{-# LANGUAGE DataKinds #-}
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
import Mog.Index (type Index)




-- * Schema datatypes

infixr 5 &
infix  6 ↦
infixr 7 %

-- | Table-group wrapper
data Schema (name :: Symbol) tables
-- | Table-group cons
data t & ts
-- | Table-group nil
data TablesEnd

-- | Table (one) wrapper
data Table (name :: Symbol) pk_v
-- | Table (one) spec
data pk ↦ v

-- | Tuple cons
data c % cs
-- | Tuple nil
data Ø

data Ref fk (index :: Index)
data Prim a

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 :: Int & Char % Word % () ↦ String % Float % () & Double
           :~: Int & (((Char % (Word % ())) ↦ (String % (Float % ()))) & Double)
_testPrec10 = Refl
