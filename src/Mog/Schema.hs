{-# LANGUAGE DataKinds #-}
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

class                                           ValidTable t                 ts
instance ValidTable pk_v ts                  => ValidTable (Table name pk_v) ts
instance (ValidCols pk   ts, ValidCols v ts) => ValidTable (pk ↦ v)          ts

class                                          ValidCols pk_v     ts
instance (ValidCol   c ts, ValidCols cs ts) => ValidCols (c % cs) ts
instance                                       ValidCols Ø        ts

class                        ValidCol c              ts
instance                     ValidCol (Prim a)       ts
instance IsFk index fk ts => ValidCol (Ref fk index) ts

-- ** Foreign keys

-- | A fk must match the pk of the table indicated by the index.
class                        IsFk (ix :: Index) k tables
instance IsFk ix k tables => IsFk ('There ix)   k (t & tables)
instance IsPk    k t      => IsFk  'Here        k (t & tables)

class                   IsPk k t
instance IsPk k pk_v => IsPk k (Table name pk_v)
instance                IsPk k (k ↦ v)
