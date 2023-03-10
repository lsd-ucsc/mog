{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
module Frontend where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Backend (Index(..))

-- type PkRel pk tuple = PkTable pk tuple

data a :@ (index :: Index) = Ref a

-- -- TODO: change Example.hs to produce this value via `qAbstraction x`
-- exampleQueue ::
--     ( Rel (Int :@ 'Here, Int :@ 'Here)
--     , Rel Int
--     )
-- exampleQueue =
--     ( Rel (Set.fromList [(Ref 4, Ref 2), (Ref 5, Ref 4)])
--     , Rel (Set.fromList [2,4,5])
--     )
