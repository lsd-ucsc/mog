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

class Mergeable a where
    -- | @merge base ours theirs@ is a family of binary merges against a
    -- base. Any @merge base@ is a commutative semigroup for
    -- versions that have @base@ as an ancestor.
    --
    -- (TODO: It might be a monoid but we'll think about the identity later.)
    merge :: a -> (a -> a -> a)
