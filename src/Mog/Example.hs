{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Examples of the internal operations; also doubles as compile-time tests of
-- the library
module Mog.Example where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))

import Mog.Schema
import Mog.Index (type Index(..))
import Mog.Instance

-- $setup
-- >>> :set -XTypeApplications
-- >>> import Mog.Output

_testValid :: ValidSchema a => Proxy a -> Proxy a
_testValid Proxy = Proxy




-- * Pair as map

type PairSchema a =
    Schema "pair"
    ( Table "map" (Prim Bool % Ø ↦ Prim a % Ø)
    & TablesEnd
    )

_testValid10 :: Proxy (PairSchema a)
_testValid10 = _testValid Proxy

_testInst25 :: Inst (PairSchema a)
           :~: Assoc (Bool :% Ø_) (a :% Ø_) :& TTablesEnd
_testInst25 = Refl




-- * Pair as singleton

type TwopleSchema a b =
    Schema "twople"
    ( Table "singleton" (Ø ↦ Prim a % Prim b % Ø)
    & TablesEnd
    )

_testValid20 :: Proxy (TwopleSchema a b)
_testValid20 = _testValid Proxy

_testInst26 :: Inst (TwopleSchema a b)
           :~: Assoc Ø_ (a :% b :% Ø_) :& TTablesEnd
_testInst26 = Refl




-- * Queue

type QueueSchema a =
    Schema "queue"
    ( Table "ob" (Ref (Prim a % Ø) 'Here % Ref (Prim a % Ø) 'Here % Ø ↦ Ø)
    & Table "mem" (Prim a % Ø ↦ Ø)
    & TablesEnd
    )

_testValid30 :: Proxy (QueueSchema a)
_testValid30 = _testValid Proxy

_testInst30 :: Inst (QueueSchema a)
           :~:   Assoc ((a :% Ø_) :% (a :% Ø_) :% Ø_) Ø_
              :& Assoc (a :% Ø_) Ø_
              :& TTablesEnd
_testInst30 = Refl

-- |
--
-- >>> :{
-- pure queueInstance == (fromOutput @(QueueSchema Int) Proxy
--                        . toOutput @(QueueSchema Int) Proxy) queueInstance
-- :}
-- True
queueInstance :: Inst (QueueSchema Int)
queueInstance =
       fromList [ (4 :% Ø_) :% (2 :% Ø_) :% Ø_
                , (5 :% Ø_) :% (4 :% Ø_) :% Ø_ ]
    :& fromList [ 2 :% Ø_
                , 4 :% Ø_
                , 5 :% Ø_ ]
    :& TTablesEnd
  where
    fromList :: [a] -> Assoc a Ø_
    fromList = map $ \x -> (x, Ø_)




-- * Ordered map

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
           :~:    Assoc ((k :% Ø_) :% (k :% Ø_) :% Ø_)
                      Ø_
               :& Assoc ((k :% Ø_) :% Ø_)
                      (v :% Ø_)
               :& Assoc (k :% Ø_)
                      Ø_
               :& TTablesEnd
_testInst40 = Refl

-- |
--
-- >>> :{
-- pure orderedMapExample == (fromOutput @(OrderedMapSchema Char Int) Proxy
--                            . toOutput @(OrderedMapSchema Char Int) Proxy) orderedMapExample
-- :}
-- True
orderedMapExample :: Inst (OrderedMapSchema Char Int)
orderedMapExample =
       fromList [ ('a' :% Ø_) :% ('b' :% Ø_) :% Ø_
                , ('b' :% Ø_) :% ('c' :% Ø_) :% Ø_
                ]
    :& [ (('a' :% Ø_) :% Ø_, 123 :% Ø_)
       , (('b' :% Ø_) :% Ø_, 456 :% Ø_)
       , (('c' :% Ø_) :% Ø_, 789 :% Ø_)
       ]
    :& fromList [ 'a' :% Ø_, 'b' :% Ø_, 'c' :% Ø_ ]
    :& TTablesEnd
  where
    fromList :: [a] -> Assoc a Ø_
    fromList = map $ \x -> (x, Ø_)
