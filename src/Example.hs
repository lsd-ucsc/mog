module Example where

import Data.Set (Set)
import qualified Data.Set as Set

import Backend

-- * Data structure

data Queue a = Queue [a]
    deriving Show

qEmpty :: Queue a
qEmpty = Queue []

qPush :: a -> Queue a -> Queue a
qPush x (Queue xs) = Queue (x:xs)

qPop :: Queue a -> Maybe (a, Queue a)
qPop (Queue []) = Nothing
qPop (Queue (x:xs)) = Just (x, Queue xs)

-- * Characteristic relations

qRmem :: Ord a => Queue a -> Set a
qRmem (Queue xs) = Set.fromList xs

qRob :: Ord a => Queue a -> Set (a, a)
qRob (Queue []) = Set.empty
qRob (Queue (x:xs)) = Set.fromList $ zip (x:xs) xs

qAbstraction :: Ord a => Queue a -> (Set (a, a), Set a)
qAbstraction q = (qRob q, qRmem q)
