{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Module for instances of a schema
module Mog.Instance
    ( (:&)(..), TTablesEnd(..)
    , (:%)(..), TTupleEnd(..)
    , Inst
    ) where

import Data.Kind (Type)
import Data.Map (Map)
import Data.Type.Equality ((:~:)(..))

import Mog.Schema




-- * Instance datatypes

infixr 5 :&
infixr 7 :%

-- | Table-group cons
data a :& b = a :& b deriving Show
-- | Table-group end
data TTablesEnd = TTablesEnd deriving Show

-- | Tuple cons
data a :% b = a :% b deriving (Show, Eq, Ord)
-- | Tuple nil
data TTupleEnd = TTupleEnd deriving (Show, Eq, Ord)




-- * Instance type for a schema

-- | Compute the type of an instance from a database schema.
type family Inst a :: Type where
    -- An instance of a schema consists of an instance for each of its tables.
    Inst (Schema name ts)  = Inst ts
    Inst (t & ts)          = Inst t :& Inst ts
    Inst TablesEnd         = TTablesEnd

    -- An instance of a table consists of a map from instances of its key columns
    -- to instances of its value columns.
    Inst (Table name pk_v) = Inst pk_v
    Inst (pk ↦ v)          = Map (Inst pk) (Inst v)

    -- An instance of a list of columns is an instance for each column.
    Inst (c % cs)          = Inst c :% Inst cs
    Inst Ø                 = TTupleEnd

    -- An instance of a primitive column is an instance of its type.
    Inst (Prim a)          = a
    -- An instance of a foreign key is an instance of the columns
    -- constituting the key for the referenced table.
    Inst (Ref fk _)        = Inst fk

_testInst10 = Refl
           :: Inst (Prim Char % Ø)
          :~: (Char :% TTupleEnd)
_testInst11 = Refl
           :: Inst (Ref (Prim Char % Ø) 'Here % Ø)
          :~: ((Char :% TTupleEnd) :% TTupleEnd)
_testInst12 = Refl
           :: Inst (Prim Int % Ref (Prim Char % Prim Word % Ø) 'Here % Prim Double % Ø)
          :~: Int :% (Char :% Word :% TTupleEnd) :% Double :% TTupleEnd
_testInst13 = Refl
           :: Inst (Prim Int % Ref (Prim Char % Prim Word % Ø) 'Here % Prim Double % Ø)
          :~: Int :% (Char :% Word :% TTupleEnd) :% Double :% TTupleEnd
_testInst20 = Refl
           :: Inst (Table "teble" (Prim Int % Ø ↦ Prim String % Ø))
          :~: Map (Int :% TTupleEnd) (String :% TTupleEnd)
