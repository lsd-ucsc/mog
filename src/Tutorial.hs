{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-} -- MaybeTuple constraint
module Tutorial where

import Codec.Serialise (Serialise)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import qualified Data.List as List
import qualified Data.Map as Map hiding (Map)
import qualified Data.Set as Set hiding (Set)

import Mog




-- * Counter

newtype Counter a = Counter a
    deriving (Eq, Show, Generic)

instance Serialise a => Serialise (Counter a)

-- |
--
-- prop> merge (Counter l) (Counter a) (Counter b) == merge (Counter l) (Counter b) (Counter a)
instance Num a => Mergeable (Counter a) where
    merge (Counter base) (Counter v1) (Counter v2) =
        Counter $ base + (v1 - base) + (v2 - base)



-- * Queue

-- ** Implementation

newtype Queue a = Queue [a]
    deriving Show

qEmpty :: Queue a
qEmpty = Queue []

qPush :: a -> Queue a -> Queue a
qPush x (Queue xs) = Queue (x:xs)

qPop :: Queue a -> Maybe (a, Queue a)
qPop (Queue []) = Nothing
qPop (Queue (x:xs)) = Just (x, Queue xs)

-- ** MRDT

-- TODO: also try the encoding that uses (Int:>a)
instance (Ord a, MaybeTuple a) => MRDT (Queue a) where
    -- Type of characteristic relations
    type Abstracted (Queue a) =
        Dt "queue"
        ( "ob"  :::: Set (a:@1, a:@1)
        , "mem" :::: Set a
        )
    -- Abstraction function
    α (Queue xs) = Dt
        ( Rel $ case Ref <$> xs of
                [] -> Set.empty
                y:ys -> Set.fromList $ zip (y:ys) ys
        , Rel $ Set.fromList xs
        )
    -- Concretization function
    γ = undefined




-- * Ordered map

-- ** Implementation

data OrderedMap k v = OrderedMap { omMap :: Map k v, omOrder :: [k] }

instance (Ord k, Show k, Show v) => Show (OrderedMap k v) where
    show om = "OrderedMap" ++ show (omList om)

omEmpty :: OrderedMap k v
omEmpty = OrderedMap Map.empty []

omLookup :: Ord k => k -> OrderedMap k v -> Maybe v
omLookup k OrderedMap{omMap} = Map.lookup k omMap

omList :: Ord k => OrderedMap k v -> [(k, v)]
omList OrderedMap{omMap,omOrder} = foldr step [] omOrder
  where
    stepMaybe k xs = Map.lookup k omMap >>= \v -> pure ((k, v) : xs)
    step k = maybe (error "bug in OrderedMap") id . stepMaybe k

-- |
--
-- Map does not maintain insertion order.
--
-- >>> Map.insert "three" 3 . Map.insert "two" 2 . Map.insert "one" 1 $ Map.empty
-- fromList [("one",1),("three",3),("two",2)]
--
-- Ordered map maintains insertion order.
--
-- >>> omInsert "three" 3 . omInsert "two" 2 . omInsert "one" 1 $ omEmpty
-- OrderedMap[("three",3),("two",2),("one",1)]
omInsert :: Ord k => k -> v -> OrderedMap k v -> OrderedMap k v
omInsert k v om@OrderedMap{omMap,omOrder} =
    om  { omMap = Map.insert k v omMap
        , omOrder = if Map.member k omMap then omOrder else k : omOrder
        }

-- |
--
-- >>> omDelete "one" . omInsert "three" 3 . omInsert "two" 2 . omInsert "one" 1 $ omEmpty
-- OrderedMap[("three",3),("two",2)]
--
-- >>> omDelete "two" . omInsert "three" 3 . omInsert "two" 2 . omInsert "one" 1 $ omEmpty
-- OrderedMap[("three",3),("one",1)]
omDelete :: Ord k => k -> OrderedMap k v -> OrderedMap k v
omDelete k om@OrderedMap{omMap,omOrder} =
    om  { omMap = Map.delete k omMap
        , omOrder = if Map.member k omMap then List.delete k omOrder else omOrder
        }

-- | Derived from other operations
--
-- >>> omPop omEmpty
-- Nothing
--
-- >>> omPop . omInsert "three" 3 . omInsert "two" 2 . omInsert "one" 1 $ omEmpty
-- Just (OrderedMap[("two",2),("one",1)],"three",3)
omPop :: Ord k => OrderedMap k v -> Maybe (OrderedMap k v, k, v)
omPop OrderedMap{omOrder=[]} = Nothing
omPop om@OrderedMap{omOrder=(k:_)} = omLookup k om >>= \v -> pure (omDelete k om, k, v)

-- ** MRDT

instance (Ord k, MaybeTuple k, MaybeTuple v) => MRDT (OrderedMap k v) where
    -- Type of characteristic relations
    type Abstracted (OrderedMap k v) =
        Dt "ordered-map"
        ( "ord"  :::: Set (k:@2, k:@2)
        , "map"  :::: k:@1 :> v
        , "keys" :::: Set k
        )
    -- Abstraction function
    α om = Dt
        ( Rel $ case Ref <$> omOrder om of
                [] -> Set.empty
                k:ks -> Set.fromList $ zip (k:ks) ks
        , Rel $ Map.mapKeys Ref $ omMap om
        , Rel $ Map.keysSet $ omMap om
        )
    -- Concretization function
    γ = undefined
