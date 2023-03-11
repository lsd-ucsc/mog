{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- extensions for the validation classes
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

  ValidSchema,
) where

import Data.Type.Equality ((:~:)(..))
import GHC.TypeLits (Symbol)




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

data Index = Here | There Index
data Ref fk (index :: Index)
data Prim a

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 :: Int & Char % Word % () ↦ String % Float % () & Double
           :~: Int & (((Char % (Word % ())) ↦ (String % (Float % ()))) & Double)
_testPrec10 = Refl




-- * Schema validation

class                          ValidSchema a
instance ValidTables tables => ValidSchema (Schema name tables)

class                                         ValidTables ts
instance (ValidTable t ts, ValidTables ts) => ValidTables (t & ts)
instance                                      ValidTables TablesEnd

class                         ValidTable t                 ts
instance ValidCols pk_v ts => ValidTable (Table name pk_v) ts

class                                          ValidCols pk_v     ts
instance (ValidCols pk ts, ValidCols  v ts) => ValidCols (pk ↦ v) ts
instance (ValidCol   c ts, ValidCols cs ts) => ValidCols (c % cs) ts
instance                                       ValidCols Ø        ts

class                      ValidCol c              ts
instance                   ValidCol (Prim a)       ts
instance Fk fk index ts => ValidCol (Ref fk index) ts

-- ** Foreign keys

-- | A fk must match the pk of the table indicated by the index.
class                      Fk k (ix :: Index) tables
instance Fk k ix tables => Fk k (There ix)    (t & tables)
instance                   Fk k  Here         (Table name (k ↦ v) & tables)
