{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Module for UI/UX reexports.
module Mog
    -- * The MRDT typeclass
    ( MRDT(..)
    -- ** Constraints for constituent types
    , MaybeTuple
    , Mergeable(..)
    -- ** Grammar of Relations
    , Dt(Dt)
    , (::::)(Rel)
    , (:>)
    , (:@)(Ref)
    ) where

import Data.Kind (Type)

import Mog.MergeDriver.Merge (Mergeable(..))
import Mog.UI

class (DtC (Abstracted a)) => MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a
