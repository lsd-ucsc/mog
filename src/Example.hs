{-# OPTIONS_GHC "-Wno-missing-signatures" #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Example where

import Data.Set (Set)
import Data.Map (Map)
import Data.Type.Equality ((:~:)(..))

import qualified Data.Set as Set
import qualified Data.Map as Map

import Mog.Schema
import Mog.Instance




-- * Pair as map

-- ** Implementation

-- TODO

-- *** Characteristic relations

-- TODO

-- ** Schema

type PairSchema a =
    Schema "pair"
    ( Table "map" (Prim Bool % Ø ↦ Prim a % Ø)
    & TablesEnd
    )

_testInst25 = Refl
           :: Inst (PairSchema a)
          :~: Map (Bool :% TTupleEnd) (a :% TTupleEnd) :& TTablesEnd




-- * Pair as singleton

-- ** Implementation

-- TODO

-- *** Characteristic relations

-- TODO

-- ** Schema

type TwopleSchema a b =
    Schema "twople"
    ( Table "singleton" (Ø ↦ Prim a % Prim b % Ø)
    & TablesEnd
    )

_testInst26 = Refl
           :: Inst (TwopleSchema a b)
          :~: Map TTupleEnd (a :% b :% TTupleEnd) :& TTablesEnd




-- * Queue

-- ** Implementation

data Queue a = Queue [a]
    deriving Show

qEmpty :: Queue a
qEmpty = Queue []

qPush :: a -> Queue a -> Queue a
qPush x (Queue xs) = Queue (x:xs)

qPop :: Queue a -> Maybe (a, Queue a)
qPop (Queue []) = Nothing
qPop (Queue (x:xs)) = Just (x, Queue xs)

-- *** Characteristic relations

qRmem :: Ord a => Queue a -> Set a
qRmem (Queue xs) = Set.fromList xs

qRob :: Ord a => Queue a -> Set (a, a)
qRob (Queue []) = Set.empty
qRob (Queue (x:xs)) = Set.fromList $ zip (x:xs) xs

qAbstraction :: Ord a => Queue a -> (Set (a, a), Set a)
qAbstraction q = (qRob q, qRmem q)

-- TODO: conversion to instance of schema

-- ** Schema

type QueueSchema a =
    Schema "queue"
    ( Table "ob" (Ref (Prim a % Ø) 'Here % Ref (Prim a % Ø) 'Here % Ø ↦ Ø)
    & Table "mem" (Prim a % Ø ↦ Ø)
    & TablesEnd
    )

_testInst30 = Refl
           :: Inst (QueueSchema a)
          :~:   Map ((a :% TTupleEnd) :% (a :% TTupleEnd) :% TTupleEnd) TTupleEnd
             :& Map (a :% TTupleEnd) TTupleEnd
             :& TTablesEnd

queueInstance :: Inst (QueueSchema Int)
queueInstance =
       fromList [ ((4 :% TTupleEnd) :% (2 :% TTupleEnd) :% TTupleEnd)
                , ((5 :% TTupleEnd) :% (4 :% TTupleEnd) :% TTupleEnd) ]
    :& fromList [ 2 :% TTupleEnd
                , 4 :% TTupleEnd
                , 5 :% TTupleEnd ]
    :& TTablesEnd
  where
    fromList :: Ord a => [a] -> Map a TTupleEnd
    fromList = Map.fromSet (const TTupleEnd) . Set.fromList

-- TODO: doctest
--    Just queueInstance
-- == ( fromSchema @(QueueSchema Int) Proxy
--    . toSchema @(QueueSchema Int) Proxy
--    ) queueInstance



-- * Ordered map

-- ** Implementation

-- TODO

-- *** Characteristic relations

-- TODO

-- ** Schema

type OrderedMapSchema k v =
    Schema "ordered-map"
    ( Table "ord" ( Ref (Prim k % Ø) ('There 'Here)
                  % Ref (Prim k % Ø) ('There 'Here)
                  % Ø
                  ↦ Ø )
    & Table "map" ( Ref (Prim k % Ø) 'Here
                  % Ø
                  ↦ Prim v
                  % Ø)
    & Table "keys" ( Prim k
                   % Ø
                   ↦ Ø)
    & TablesEnd
    )

_testInst40 = Refl
           :: Inst (OrderedMapSchema k v)
          :~:    Map ((k :% TTupleEnd) :% (k :% TTupleEnd) :% TTupleEnd)
                     TTupleEnd
              :& Map ((k :% TTupleEnd) :% TTupleEnd)
                     (v :% TTupleEnd)
              :& Map (k :% TTupleEnd)
                     TTupleEnd
              :& TTablesEnd

orderedMapExample :: Inst (OrderedMapSchema Char Int)
orderedMapExample =
           fromList [ ('a' :% TTupleEnd) :% ('b' :% TTupleEnd) :% TTupleEnd
                    , ('b' :% TTupleEnd) :% ('c' :% TTupleEnd) :% TTupleEnd
                    ]
    :& Map.fromList [ (('a' :% TTupleEnd) :% TTupleEnd, 123 :% TTupleEnd)
                    , (('b' :% TTupleEnd) :% TTupleEnd, 456 :% TTupleEnd)
                    , (('c' :% TTupleEnd) :% TTupleEnd, 789 :% TTupleEnd)
                    ]
    :&     fromList [ 'a' :% TTupleEnd, 'b' :% TTupleEnd, 'c' :% TTupleEnd ]
    :& TTablesEnd
  where
    fromList :: Ord a => [a] -> Map a TTupleEnd
    fromList = Map.fromSet (const TTupleEnd) . Set.fromList

-- TODO: doctest
--     Just orderedMapExample
-- == ( fromSchema @(OrderedMapSchema Char Int) Proxy
--    . toSchema @(OrderedMapSchema Char Int) Proxy
--    ) orderedMapExample
