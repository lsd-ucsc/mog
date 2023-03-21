{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- extensions for the validation classes
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module for instances of a schema
module Mog.Instance
    ( (:&)(..), TTablesEnd(..)
    , (:%)(..), Ø_(..)
    , Assoc
    , Inst
    , ValidSchema(..)
    ) where

import Data.Kind (Type)
import Data.Map (Map)
import Data.Type.Equality ((:~:)(..))
import Data.Proxy (Proxy(..))

import Mog.Schema




-- * Instance datatypes

infixr 5 :&
infixr 7 :%

-- | Table-group cons
data a :& b = a :& b deriving (Show, Eq)
-- | Table-group end
data TTablesEnd = TTablesEnd deriving (Show, Eq)

-- | Tuple cons
data a :% b = a :% b deriving (Show, Eq, Ord)
-- | Tuple nil
data Ø_ = Ø_ deriving (Show, Eq, Ord)

-- | Tuples are an easily mappable association of keys to values.
type Assoc k v = [(k, v)]




-- * Instance type for a schema

-- | Compute the type of an instance from a database schema.
type family Inst a :: Type where
    -- An instance of a schema consists of an instance for each of its tables.
    Inst (Schema name ts)  = Inst ts
    Inst (t & ts)          = Inst t :& Inst ts
    Inst TablesEnd         = TTablesEnd

    -- An instance of a table consists of an assoc from instances of its key
    -- columns to instances of its value columns.
    Inst (Table name pk_v) = Inst pk_v
    Inst (pk ↦ v)          = Assoc (Inst pk) (Inst v)

    -- An instance of a list of columns is an instance for each column.
    Inst (c % cs)          = Inst c :% Inst cs
    Inst Ø                 = Ø_

    -- An instance of a primitive column is an instance of its type.
    Inst (Prim a)          = a
    -- An instance of a foreign key is an instance of the columns
    -- constituting the key for the referenced table.
    Inst (Ref fk _)        = Inst fk

_testInst10 :: Inst (Prim Char % Ø)
           :~: (Char :% Ø_)
_testInst10 = Refl

_testInst11 :: Inst (Ref (Prim Char % Ø) 'Here % Ø)
           :~: ((Char :% Ø_) :% Ø_)
_testInst11 = Refl

_testInst12 :: Inst (Prim Int % Ref (Prim Char % Prim Word % Ø) 'Here % Prim Double % Ø)
           :~: Int :% (Char :% Word :% Ø_) :% Double :% Ø_
_testInst12 = Refl

_testInst13 :: Inst (Prim Int % Ref (Prim Char % Prim Word % Ø) 'Here % Prim Double % Ø)
           :~: Int :% (Char :% Word :% Ø_) :% Double :% Ø_
_testInst13 = Refl

_testInst20 :: Inst (Table "teble" (Prim Int % Ø ↦ Prim String % Ø))
           :~: Assoc (Int :% Ø_) (String :% Ø_)
_testInst20 = Refl




-- * Schema validation

class ValidSchema a where
    pruneSchema :: Proxy a -> Inst a -> Inst a
instance ValidTables tables => ValidSchema (Schema name tables) where
    pruneSchema Proxy x = pruneTables @tables Proxy x

class ValidTables ts where
    pruneTables :: Proxy ts -> Inst ts -> Inst ts
instance (ValidTable t ts, ValidTables ts) => ValidTables (t & ts) where
    -- | Prune referenced tables first. Consider this example:
    --
    --   "foo" {a@1@1, b@1@1, c@1@1}
    -- & "bar" {a@1, b@1, c@1}
    -- & "baz" {a, b}
    -- & End
    --
    -- Here we should prune "bar" of c@1 first by observing "baz", so that
    -- "foo" can learn that c@1@1 is absent.
    pruneTables Proxy (x :& xs) =
        let xs' = pruneTables @ts Proxy xs in
        pruneTable @t @ts Proxy Proxy x xs' :& xs'
instance ValidTables TablesEnd where
    pruneTables Proxy TTablesEnd = TTablesEnd

class ValidTable t ts where
    pruneTable :: Proxy t -> Proxy ts -> Inst t -> Inst ts -> Inst t
instance ValidTable pk_v ts => ValidTable (Table name pk_v) ts where
    pruneTable Proxy Proxy x xs = pruneTable @pk_v @ts Proxy Proxy x xs
instance (ValidCols pk ts, ValidCols v ts) => ValidTable (pk ↦ v) ts where
    pruneTable Proxy Proxy x xs = error "not yet implemented"

class ValidCols pk_v ts
instance (ValidCol c ts, ValidCols cs ts) => ValidCols (c % cs) ts
instance ValidCols Ø ts

class ValidCol c ts
instance ValidCol (Prim a) ts
instance IsFk index fk ts => ValidCol (Ref fk index) ts

-- ** Foreign keys

-- | A fk must match the pk of the table indicated by the index.
class                        IsFk (ix :: Index) k tables
instance IsFk ix k tables => IsFk ('There ix)   k (t & tables)
instance IsPk    k t      => IsFk  'Here        k (t & tables)

class                   IsPk k t
instance IsPk k pk_v => IsPk k (Table name pk_v)
instance                IsPk k (k ↦ v)
