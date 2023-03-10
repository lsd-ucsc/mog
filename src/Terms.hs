{-# LANGUAGE TypeOperators #-}
module Terms (
  (:&)(..),
  (:%)(..),
  TTablesEnd(..),
  TTupleEnd(..)
) where

infixr 5 :&
infixr 7 :%

data a :& b = a :& b deriving Show -- ^ Table-group cons
data TTablesEnd = TTablesEnd deriving Show

data a :% b = a :% b deriving (Show, Eq, Ord)
data TTupleEnd = TTupleEnd deriving (Show, Eq, Ord)
