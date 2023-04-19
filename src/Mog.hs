{-# LANGUAGE TypeFamilies #-}

-- | Module for UI/UX reexports.
module Mog
    ( MRDT(..)
    , Dt(Dt)
    , (::::)(Rel)
    , (:>)
    , (:@)(Ref)
    , Mergeable(..)
    ) where

import Mog.UI
import Data.Kind (Type)
import Mog.MergeDriver.Merge (Mergeable(..))

class {-Relations (Abstracted a) =>-} MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a

