{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Example where

import Data.Set (Set)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))

import qualified Data.Set as Set
import qualified Data.Map as Map

import Mog.Schema
import Mog.Instance

-- $setup
-- >>> :set -XTypeApplications
-- >>> import Mog.Output

_testValid :: ValidSchema a => Proxy a -> Proxy a
_testValid Proxy = Proxy




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

_testValid10 :: Proxy (PairSchema a)
_testValid10 = _testValid Proxy

_testInst25 :: Inst (PairSchema a)
           :~: Map (Bool :% Ø_) (a :% Ø_) :& TTablesEnd
_testInst25 = Refl




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

_testValid20 :: Proxy (TwopleSchema a b)
_testValid20 = _testValid Proxy

_testInst26 :: Inst (TwopleSchema a b)
           :~: Map Ø_ (a :% b :% Ø_) :& TTablesEnd
_testInst26 = Refl




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

_testValid30 :: Proxy (QueueSchema a)
_testValid30 = _testValid Proxy

_testInst30 :: Inst (QueueSchema a)
           :~:   Map ((a :% Ø_) :% (a :% Ø_) :% Ø_) Ø_
              :& Map (a :% Ø_) Ø_
              :& TTablesEnd
_testInst30 = Refl

-- |
--
-- >>> :{
-- Just queueInstance == (fromOutput @(QueueSchema Int) Proxy
--                        . toOutput @(QueueSchema Int) Proxy) queueInstance
-- :}
-- True
queueInstance :: Inst (QueueSchema Int)
queueInstance =
       fromList [ ((4 :% Ø_) :% (2 :% Ø_) :% Ø_)
                , ((5 :% Ø_) :% (4 :% Ø_) :% Ø_) ]
    :& fromList [ 2 :% Ø_
                , 4 :% Ø_
                , 5 :% Ø_ ]
    :& TTablesEnd
  where
    fromList :: Ord a => [a] -> Map a Ø_
    fromList = Map.fromSet (const Ø_) . Set.fromList




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

_testValid40 :: Proxy (OrderedMapSchema k v)
_testValid40 = _testValid Proxy

_testInst40 :: Inst (OrderedMapSchema k v)
           :~:    Map ((k :% Ø_) :% (k :% Ø_) :% Ø_)
                      Ø_
               :& Map ((k :% Ø_) :% Ø_)
                      (v :% Ø_)
               :& Map (k :% Ø_)
                      Ø_
               :& TTablesEnd
_testInst40 = Refl

-- |
--
-- >>> :{
-- Just orderedMapExample == (fromOutput @(OrderedMapSchema Char Int) Proxy
--                            . toOutput @(OrderedMapSchema Char Int) Proxy) orderedMapExample
-- :}
-- True
orderedMapExample :: Inst (OrderedMapSchema Char Int)
orderedMapExample =
           fromList [ ('a' :% Ø_) :% ('b' :% Ø_) :% Ø_
                    , ('b' :% Ø_) :% ('c' :% Ø_) :% Ø_
                    ]
    :& Map.fromList [ (('a' :% Ø_) :% Ø_, 123 :% Ø_)
                    , (('b' :% Ø_) :% Ø_, 456 :% Ø_)
                    , (('c' :% Ø_) :% Ø_, 789 :% Ø_)
                    ]
    :&     fromList [ 'a' :% Ø_, 'b' :% Ø_, 'c' :% Ø_ ]
    :& TTablesEnd
  where
    fromList :: Ord a => [a] -> Map a Ø_
    fromList = Map.fromSet (const Ø_) . Set.fromList
