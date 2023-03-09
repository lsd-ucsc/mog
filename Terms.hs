{-# LANGUAGE TypeOperators #-}
module Terms where

data a :& b = a :& b deriving (Show) -- ^ Table-group cons
infixr 5 :&
data TTablesEnd = TTablesEnd

data a :% b = a :% b deriving (Show, Eq, Ord)
infixr 7 :%
data TTupleEnd = TTupleEnd
