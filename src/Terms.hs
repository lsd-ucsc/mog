{-# LANGUAGE TypeOperators #-}
module Terms where

data a :& b = a :& b deriving Show -- ^ Table-group cons
infixr 5 :&
data TTablesEnd = TTablesEnd deriving Show

data a :% b = a :% b deriving (Show, Eq, Ord)
infixr 7 :%
data TTupleEnd = TTupleEnd deriving (Eq, Ord)
