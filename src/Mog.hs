{-# LANGUAGE TypeFamilies #-}

-- | Module for UI/UX reexports.
module Mog
    ( MRDT(..)
    , Dt(Dt)
    , (::::)(Rel)
    , (:>)
    , (:@)(Ref)
    , Mergeable(..)
    , MaybeTuple
    ) where

import Data.Kind (Type)
import Mog.UI
import Mog.MergeDriver.Merge (Mergeable(..))

class (DtC (Abstracted a)) => MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a

