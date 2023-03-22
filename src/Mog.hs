{-# LANGUAGE TypeFamilies #-}

-- | Module for UI/UX reexports.
module Mog
    ( MRDT(..)
    , Dt(Dt)
    , (::::)(Rel)
    , (:>)
    , (:@)(Ref)
    ) where

import Mog.UI
import Data.Kind (Type)

class {-Relations (Abstracted a) =>-} MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a
    -- three-way merge
