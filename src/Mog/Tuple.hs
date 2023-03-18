{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, DataKinds #-} -- TypeFamilies implies KindSignatures
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Inductive tuple-lists
module Mog.Tuple where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))

-- | Singleton tuple (this project uses 8.10.7 which doesn't have
-- Data.Tuple.Solo iirc).
newtype T a = T { unT :: a } deriving (Show, Read, Eq, Ord)

-- | True up to tuples of length 20. Anything else is NotATuple™.
type family IsTuple a :: Bool where
    IsTuple () = 'True
    IsTuple(T a) = 'True
    IsTuple (a, b) = 'True
    IsTuple (a, b, c) = 'True
    IsTuple (a, b, c, d) = 'True
    IsTuple (a, b, c, d, e) = 'True
    IsTuple (a, b, c, d, e, f) = 'True
    IsTuple (a, b, c, d, e, f, g) = 'True
    IsTuple (a, b, c, d, e, f, g, h) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = 'True
    IsTuple (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = 'True
    --                     5             10             15             20
    IsTuple a = 'False

-- | Class to convert data to tuples. Tuples are unchanged, non-tuples are
-- wrapped with 'T'.
class (b ~ IsTuple a) => ToTuple (b::Bool) a where
    type TupleOf b a :: Type
    toTupleP :: Proxy b -> a -> TupleOf b a
instance ('True ~ IsTuple a) => ToTuple 'True a where
    type TupleOf 'True a = a
    toTupleP Proxy = id
instance ('False ~ IsTuple a) => ToTuple 'False a where
    type TupleOf 'False a = T a
    toTupleP Proxy = T

-- | Convenience function for the 'ToTuple' class which doesn't require a proxy
-- argument.
toTuple :: forall b a. (ToTuple b a) => a -> TupleOf b a
toTuple =  toTupleP @b Proxy

-- | Tuples arranged into lists we can induct over.
type family TupleList a :: Type where
    TupleList () = ()
    TupleList(T a) = (a, ())
    TupleList (a, b) = (a, (b, ()))
    TupleList (a, b, c) = (a, (b, (c, ())))
    TupleList (a, b, c, d) = (a, (b, (c, (d, ()))))
    TupleList (a, b, c, d, e) = (a, (b, (c, (d, (e, ())))))
    TupleList (a, b, c, d, e, f) = (a, (b, (c, (d, (e, (f, ()))))))
    TupleList (a, b, c, d, e, f, g) = (a, (b, (c, (d, (e, (f, (g, ())))))))
    TupleList (a, b, c, d, e, f, g, h) = (a, (b, (c, (d, (e, (f, (g, (h, ()))))))))
    TupleList (a, b, c, d, e, f, g, h, i) = (a, (b, (c, (d, (e, (f, (g, (h, (i, ())))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, ()))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, ())))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, ()))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, ())))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, ()))))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, ())))))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, ()))))))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, ())))))))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, ()))))))))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, ())))))))))))))))))))
    TupleList (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, (p, (q, (r, (s, (t, ()))))))))))))))))))))
    --                     5             10             15             20

-- | Class to convert tuples to/from tuple-lists.
class ('True ~ IsTuple a) => TupleToList a where
    toTTL :: a -> TupleList a
    fromTTL :: TupleList a -> a
instance TupleToList () where
    toTTL () = ()
    fromTTL () = ()
instance TupleToList (T a) where
    toTTL (T a) = (a,())
    fromTTL (a,()) = T a
instance TupleToList (a,b) where
    toTTL (a,b) = (a,(b,()))
    fromTTL (a,(b,())) = (a,b)
instance TupleToList (a,b,c) where
    toTTL (a,b,c) = (a,(b,(c,())))
    fromTTL (a,(b,(c,()))) = (a,b,c)
instance TupleToList (a,b,c,d) where
    toTTL (a,b,c,d) = (a,(b,(c,(d,()))))
    fromTTL (a,(b,(c,(d,())))) = (a,b,c,d)
instance TupleToList (a,b,c,d,e) where
    toTTL (a,b,c,d,e) = (a,(b,(c,(d,(e,())))))
    fromTTL (a,(b,(c,(d,(e,()))))) = (a,b,c,d,e)
instance TupleToList (a,b,c,d,e,f) where
    toTTL (a,b,c,d,e,f) = (a,(b,(c,(d,(e,(f,()))))))
    fromTTL (a,(b,(c,(d,(e,(f,())))))) = (a,b,c,d,e,f)
-- TODO: more instances for bigger tuples

-- | Class to convert data (via tuple conversion) to/from tuple-lists.
--
-- >>> toTL "hi"
-- ("hi",())
-- >>> toTL ("hi", 'Q')
-- ("hi",('Q',()))
-- >>> toTL ("hi", 'Q', 3)
-- ("hi",('Q',(3,())))
--
-- >>> fromTL ("hi",()) :: String
-- "hi"
-- >>> fromTL ("hi",('Q',())) :: (String, Char)
-- ("hi",'Q')
-- >>> fromTL ("hi",('Q',(3,()))) :: (String, Char, Int)
-- ("hi",'Q',3)
class (b ~ IsTuple a) => ToTupleList (b::Bool) a where
    toTL :: a -> TupleList (TupleOf b a)
    fromTL :: TupleList (TupleOf b a) -> a
instance ('True ~ IsTuple a, TupleToList a) => ToTupleList 'True a where
    toTL = toTTL
    fromTL = fromTTL
instance ('False ~ IsTuple a) => ToTupleList 'False a where
    toTL = toTTL . toTuple
    fromTL = unT . fromTTL

-- | Size of a tuple-list.
class TupleListSize a where
    sizeTL :: Integral b => a -> b
    sizeTLp :: Integral b => Proxy a -> b
instance TupleListSize () where
    sizeTL () = 0
    sizeTLp _ = 0
instance TupleListSize xs => TupleListSize (x, xs) where
    sizeTL (_,xs) = 1 + sizeTL xs
    sizeTLp _ = 1 + sizeTLp @xs Proxy

-- TODO: write down constraints that callers will need to use
