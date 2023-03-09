{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE KindSignatures, PolyKinds #-}
module MOG where

-- import Data.Kind

import Backend
import Frontend

class Relations a where

class Relations b => Galois a b where
    α :: a -> b
    γ :: b -> a
