{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | Quick and dirty conversion from tuples to :& and :% lists via tuple-lists.
module Mog.Tuple.HList where

import Data.Kind (Type)

import Mog.Tuple
import Mog.Instance ((:&)(..), TTablesEnd(..), (:%)(..), Ø_(..))

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeFamilies




-- * Relation lists

class TupleToRelList a where
    type RelList a :: Type
    toTTRL :: a -> RelList a
    fromTTRL :: RelList a -> a
instance TupleToRelList () where
    type RelList () = TTablesEnd
    toTTRL () = TTablesEnd
    fromTTRL TTablesEnd = ()
instance TupleToRelList xs => TupleToRelList (x, xs) where
    type RelList (x, xs) = x :& RelList xs
    toTTRL (x, xs) = x :& toTTRL xs
    fromTTRL (x :& xs) = (x, fromTTRL xs)

-- |
--
-- >>> toRL ("hello", 4, 'x')
-- "hello" :& (4 :& ('x' :& TTablesEnd))
toRL::
    ( ToTupleList b a
    , TupleToRelList (TupleList (TupleOf b a))
    ) =>
    a -> RelList (TupleList (TupleOf b a))
toRL = toTTRL . toTL

-- |
--
-- >>> fromRL $ "hello" :& 4 :& 'x' :& TTablesEnd :: (String, Int, Char)
-- ("hello",4,'x')
fromRL::
    ( ToTupleList b a
    , TupleToRelList (TupleList (TupleOf b a))
    ) =>
    RelList (TupleList (TupleOf b a)) -> a
fromRL =  fromTL . fromTTRL




-- * Column lists

class TupleToColList a where
    type ColList a :: Type
    toTTCL :: a -> ColList a
    fromTTCL :: ColList a -> a
instance TupleToColList () where
    type ColList () = Ø_
    toTTCL () = Ø_
    fromTTCL Ø_ = ()
instance TupleToColList xs => TupleToColList (x, xs) where
    type ColList (x, xs) = x :% ColList xs
    toTTCL (x, xs) = x :% toTTCL xs
    fromTTCL (x :% xs) = (x, fromTTCL xs)

-- |
--
-- >>> toCL ("hello", 4, 'x')
-- "hello" :% (4 :% ('x' :% Ø_))
toCL::
    ( ToTupleList b a
    , TupleToColList (TupleList (TupleOf b a))
    ) =>
    a -> ColList (TupleList (TupleOf b a))
toCL = toTTCL . toTL

-- |
--
-- >>> fromCL $ "hello" :% 4 :% 'x' :% Ø_ :: (String, Int, Char)
-- ("hello",4,'x')
fromCL::
    ( ToTupleList b a
    , TupleToColList (TupleList (TupleOf b a))
    ) =>
    ColList (TupleList (TupleOf b a)) -> a
fromCL =  fromTL . fromTTCL
