{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
module Mog.UI where

import Control.Monad ((<=<))
import Control.Applicative (liftA2)
import Data.Kind (Type, Constraint)
import Data.Map.Strict (Map)
import Data.Bifunctor (bimap)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import GHC.TypeLits (type Nat, type Symbol, KnownSymbol, symbolVal)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Mog.Schema (Index(..))
import Mog.Output (Row, Col)

newtype T a = T { unT :: a } deriving (Show, Read, Eq, Ord)

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

-- | Class to convert data to tuples.
class (b ~ IsTuple a) => ToTuple (b::Bool) a where
    type TupleOf b a :: Type
    toTupleP :: Proxy b -> a -> TupleOf b a
instance ('True ~ IsTuple a) => ToTuple 'True a where
    type TupleOf 'True a = a
    toTupleP Proxy = id
instance ('False ~ IsTuple a) => ToTuple 'False a where
    type TupleOf 'False a = T a
    toTupleP Proxy = T

toTuple :: forall b a. (ToTuple b a) => a -> TupleOf b a
toTuple =  toTupleP @b Proxy

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

-- TODO TESTS

-- | Class to convert data to/from tuple-lists by first converting the data
-- to/from tuples.
class (b ~ IsTuple a) => ToTupleList (b::Bool) a where
    toTL :: a -> TupleList (TupleOf b a)
    fromTL :: TupleList (TupleOf b a) -> a
instance ('True ~ IsTuple a, TupleToList a) => ToTupleList 'True a where
    toTL = toTTL
    fromTL = fromTTL
instance ('False ~ IsTuple a) => ToTupleList 'False a where
    toTL = toTTL . toTuple
    fromTL = unT . fromTTL

-- class TupleSize a           where tupleSize :: ('True ~ IsTuple a, Integral b) => Proxy a -> b
-- instance TupleSize () where tupleSize _ = 0
-- instance TupleSize (Tup1 a) where tupleSize _ = 1
-- instance TupleSize (a, b) where tupleSize _ = 2
-- instance TupleSize (a, b, c) where tupleSize _ = 3
-- instance TupleSize (a, b, c, d) where tupleSize _ = 4
-- instance TupleSize (a, b, c, d, e, f) where tupleSize _ = 5
-- instance TupleSize (a, b, c, d, e, f, g, h) where tupleSize _ = 6
-- instance TupleSize (a, b, c, d, e, f, g, h, i) where tupleSize _ = 7
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j) where tupleSize _ = 8
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k) where tupleSize _ = 9
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l) where tupleSize _ = 10
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m) where tupleSize _ = 11
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where tupleSize _ = 12
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where tupleSize _ = 13
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where tupleSize _ = 14
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) where tupleSize _ = 15
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) where tupleSize _ = 16
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) where tupleSize _ = 17
-- instance TupleSize (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) where tupleSize _ = 18



-- | Exists for documentation
type family NatIndex (a :: Nat) :: Index where
    NatIndex 0 =                                                                         'Here
    NatIndex 1 =                                                                 ('There 'Here)
    NatIndex 2 =                                                         ('There ('There 'Here))
    NatIndex 3 =                                                 ('There ('There ('There 'Here)))
    NatIndex 4 =                                         ('There ('There ('There ('There 'Here))))
    NatIndex 5 =                                 ('There ('There ('There ('There ('There 'Here)))))
    NatIndex 6 =                         ('There ('There ('There ('There ('There ('There 'Here))))))
    NatIndex 7 =                 ('There ('There ('There ('There ('There ('There ('There 'Here)))))))
    NatIndex 8 =         ('There ('There ('There ('There ('There ('There ('There ('There 'Here))))))))
    NatIndex 9 = ('There ('There ('There ('There ('There ('There ('There ('There ('There 'Here)))))))))
--  NatIndex _ = GHC.TypeLits.TypeError (GHC.TypeLits.Text "too many") -- UndecidableInstances

-- | @RefIndex n ≡ NatIndex (n - 1)@ so that you can say @1@ to mean the next
-- relation, and @2@ to mean /two/ relations over. Accordingly @0@ isn't here
-- because you can't have a fk to yourself.
type family RefIndex (a :: Nat) :: Index where
    RefIndex 1 =                                                                 'Here
    RefIndex 2 =                                                         ('There 'Here)
    RefIndex 3 =                                                 ('There ('There 'Here))
    RefIndex 4 =                                         ('There ('There ('There 'Here)))
    RefIndex 5 =                                 ('There ('There ('There ('There 'Here))))
    RefIndex 6 =                         ('There ('There ('There ('There ('There 'Here)))))
    RefIndex 7 =                 ('There ('There ('There ('There ('There ('There 'Here))))))
    RefIndex 8 =         ('There ('There ('There ('There ('There ('There ('There 'Here)))))))
    RefIndex 9 = ('There ('There ('There ('There ('There ('There ('There ('There 'Here))))))))
--  RefIndex _ = GHC.TypeLits.TypeError (GHC.TypeLits.Text "too many") -- UndecidableInstances




-- * UI Schema datatypes

infix  5 ∷
infixr 6 :~>
infixr 7 :@

-- | A named datatype with a tuple of relations.
newtype Dt (name :: Symbol) rel_tuple = Dt rel_tuple

-- | A named relation with its tuple type
newtype (name :: Symbol) ∷ pk_v = Rel pk_v

-- | A primary key in a relation tuple.
type pk :~> v = Map pk v

-- | A foreign key in a relation tuple.
type a :@ (index :: Nat) = a




-- * Internal UI→Schema classes

data RestoreError
    = WrongDatatypeName
    | WrongRelationName
    | TooFewRelations
    | TooManyRelations
    | TooFewElements
    | TooManyElements
type Option = Either RestoreError

--convert-- type family IR a :: Type where
--convert--     IR (Dt name rels) = Schema name (IR rels)
--convert-- --  IR (Rs_ (a, b, c)) = IR (R_ a) & IR (R_ b) & IR (R_ c) & TablesEnd
--convert-- ----IR (a, b)    = IR (R_ a) & IR (R_ b) & TablesEnd
--convert-- ----IR (Rel name pk_v)) = IR (R_ (Rel name pk_v)) & TablesEnd
--convert-- ----IR (pk :~> v))      = IR (R_ (Rel name pk_v) & TablesEnd
--convert-- ----IR (Set pk))        = IR (R_ (Rel name pk_v) & TablesEnd
--convert--     IR (Rel name pk_v) = Table name (IR pk_v)
--convert--     IR (pk :~> v)      = IR pk ↦ IR v
--convert--     IR (Set pk)        = IR pk ↦ Ø

-- class (IR u ~ s, Inst s ~ i) => Convert u s i where
--     convertTo :: u -> i
--     convertFrom :: i -> Option u

--convert-- --instance (Convert rels rels' rels'') =>
--convert-- --    Convert (Dt name rels) (Schema name rels') rels'' where
--convert-- --    convertTo (Dt x) = convertTo x
--convert-- --    convertFrom x = Dt <$> convertFrom x
--convert-- instance (Convert rels rels' rels'') =>
--convert--     Convert (Dt name rels) (Schema name rels') rels'' where
--convert-- --  convertTo (Dt x) = convertTo x
--convert-- --  convertFrom x = Dt <$> convertFrom x
--convert-- 
--convert-- instance (Convert pk_v pk_v' pk_v'') =>
--convert--     Convert (Rel name pk_v) (Table name pk_v') pk_v'' where
--convert--     convertTo (Rel x) = convertTo x
--convert--     convertFrom x = Rel <$> convertFrom x
--convert-- 
--convert-- instance (Convert pk pk' pk'', Convert v v' v'', Ord pk, Ord pk'') =>
--convert--     Convert (pk :~> v) (pk' ↦ v') (Map pk'' v'') where
--convert--     convertTo
--convert--         = Map.map convertTo
--convert--         . Map.mapKeys convertTo
--convert--     convertFrom
--convert--         = fmap Map.fromList
--convert--         . mapM (\(a, b) -> liftA2 (,) (convertFrom a) (convertFrom b))
--convert--         . Map.toList
--convert-- 
--convert-- instance (Convert pk pk' pk'', Ord pk, Ord pk'') =>
--convert--     Convert (Set pk) (pk' ↦ Ø) (Map pk'' Ø_) where
--convert--     convertTo
--convert--         = Map.fromSet (const Ø_)
--convert--         . Set.map convertTo
--convert--     convertFrom
--convert--         = fmap Set.fromList
--convert--         . mapM convertFrom
--convert--         . Map.keys

--  type RelIR (pk :~> v) = EltsIR pk ↦ EltsIR v
--  relInst
--      = fmap eltsInst
--      . Map.mapKeys eltsInst

---- --type family IR a :: Type where
---- ----  -- Otherwise go through RelsIR to deal with tuples of various lengths
---- ----  IR (Dt name rels)            = Schema name (IR rels)
---- --
---- --    IR (Dt name rels) = Schema name (IR (Rs_ rels))
---- --
---- --    IR (Rs_ (a, b, c)) = IR (R_ a) & IR (R_ b) & IR (R_ c) & TablesEnd
---- --    IR (Rs_ (a, b))    = IR (R_ a) & IR (R_ b) & TablesEnd
---- ----  IR (Rs_ a)         = IR (R_ a) & TablesEnd
---- --    IR (Rs_ (Rel name pk_v)) = IR (R_ (Rel name pk_v)) & TablesEnd
---- ----  IR (Rs_ (pk :~> v))      = IR (R_ (Rel name pk_v) & TablesEnd
---- ----  IR (Rs_ (Set pk))        = IR (R_ (Rel name pk_v) & TablesEnd
---- --
---- ----  IR (Rel
---- --
---- --newtype Rs_ a = Rs_ a
---- --newtype R_ a = R_ a

class Named a where
    name :: Proxy a -> String
instance (KnownSymbol s) => Named (Dt s rels) where
    name _ = symbolVal @s Proxy
instance (KnownSymbol s) => Named (s ∷ pk_v) where
    name _ = symbolVal @s Proxy


type Datatype = (String, Relations)
type Relations = [Relation]
type Relation = (String, Tuples)
type Tuples = [Tuple]
type Tuple = (Row, Row)


class DtC a where
    storeDt :: a -> Datatype
    loadDt :: Datatype -> Option a
class RelsC a where
    storeRels :: a -> Relations
    loadRels :: Relations -> Option a

class RelC a where
    storeRel :: a -> Relation
    loadRel :: Relation -> Option a
class TupsC a where
    storeTups :: a -> Tuples
    loadTups :: Tuples -> Option a

class EltsC a where
    storeElts :: a -> Row
    loadElts :: Row -> Option a
--class EltsC a where
--    storeElt :: a -> Col
--    loadElt :: Col -> a




-- TODO: make this functions instead of a class?
instance (RelsC rels, KnownSymbol name) => DtC (Dt name rels) where
    storeDt (Dt x) = (symbolVal @name Proxy, storeRels x)
    loadDt (n, x)
        | n == symbolVal @name Proxy = Dt <$> loadRels x
        | otherwise = Left WrongDatatypeName

-- relation-tuple
instance (RelC a, RelsC (b,c,d)) => RelsC (a,b,c,d) where
    storeRels (a,b,c,d) = storeRel a : storeRels (b,c,d)
    loadRels (  []) = Left TooFewRelations
    loadRels (x:xs) = liftA2 (\a (b,c,d) -> (a,b,c,d)) (loadRel x) (loadRels xs)
instance (RelC a, RelsC (b,c)) => RelsC (a,b,c) where
    storeRels (a,b,c) = storeRel a : storeRels (b,c)
    loadRels (  []) = Left TooFewRelations
    loadRels (x:xs) = liftA2 (\a (b,c) -> (a,b,c)) (loadRel x) (loadRels xs)
instance (RelC a, RelC b) => RelsC (a, b) where
    storeRels (a,b) = storeRel a : storeRel b : []
    loadRels        [] = Left TooFewRelations
    loadRels       [_] = Left TooFewRelations
    loadRels     [x,y] = liftA2 (,) (loadRel x) (loadRel y)
    loadRels (_:_:_:_) = Left TooManyRelations
-- singleton relation-tuple
instance (TupsC pk_v, KnownSymbol name) => RelsC (name ∷ pk_v) where
    storeRels x = [storeRelation storeTups x]
    loadRels      [] = Left TooFewRelations
    loadRels     [x] = loadRelation loadTups x
    loadRels (_:_:_) = Left TooManyRelations

-- relation "header"
instance (TupsC pk_v, KnownSymbol name) => RelC (name ∷ pk_v) where
    storeRel = storeRelation storeTups
    loadRel = loadRelation loadTups

storeRelation :: forall a name. KnownSymbol name => (a -> Tuples) -> name ∷ a -> Relation
storeRelation f (Rel x) = (symbolVal @name Proxy, f x)

loadRelation :: forall a name. KnownSymbol name => (Tuples -> Option a) -> Relation -> Option (name ∷ a)
loadRelation f (n, x)
    | n == symbolVal @name Proxy = Rel <$> f x
    | otherwise = Left WrongRelationName

-- relation tuples
instance (EltsC pk, EltsC v, Ord pk) => TupsC (pk :~> v) where
    storeTups
        = map (\(pk,v) -> let pk' = storeElts pk in (pk', pk' <> storeElts v))
        . Map.toList
        . id -- TODO: Fk-tuple-filter here using Map.restrictKeys
--  loadTups
--      = fmap (Map.fromListWith const) -- TODO: combine values for duplicate keys here using user-provided function
--      . mapM _
instance (EltsC pk) => TupsC (Set pk) where
    storeTups
        = map (\pk -> let pk' = storeElts pk in (pk', pk'))
        . Set.toList
        . id -- TODO: Fk-tuple-filter here using Set.intersection
--  loadTups
--      = id -- TODO: Fk-tuple-filter here using Set.intersection
--      . _1
--      . mapM _2
--  storeRel
--      = Map.map eltsGit
--      . Map.mapKeys _2
-- --  loadRel
-- --      = fmap Map.fromList
-- --      . mapM (\(a, b) -> liftA2 (,) (convertFrom a) (convertFrom b))
-- --      . Map.toList


--indexdfamily-- data Tag
--indexdfamily--     = D_ | Rs_ -- Datatype has relations
--indexdfamily--     | R_ | Es_ -- Relation has elements
--indexdfamily--     | E_
--indexdfamily-- 
--indexdfamily-- type family IR (t :: Tag) a :: Type where
--indexdfamily--     IR D_  (Dt name rels)  = Schema name (IR Rs_ rels)
--indexdfamily-- 
--indexdfamily--     IR Rs_ (a, b, c, d)    = IR R_ a & IR R_ b & IR R_ c & IR R_ d & TablesEnd
--indexdfamily--     IR Rs_ (a, b, c)       = IR R_ a & IR R_ b & IR R_ c & TablesEnd
--indexdfamily--     IR Rs_ (a, b)          = IR R_ a & IR R_ b & TablesEnd
--indexdfamily-- ----IR Rs_ a               = IR R_ a & TablesEnd -- UndecidableInstances
--indexdfamily--     IR Rs_ (Rel name pk_v) = Table name (IR R_ pk_v) & TablesEnd
--indexdfamily--     IR Rs_ (pk :~> v)      = IR Es_ pk ↦ IR Es_ v    & TablesEnd
--indexdfamily--     IR Rs_ (Set pk)        = IR Es_ pk ↦ Ø           & TablesEnd
--indexdfamily-- 
--indexdfamily--     IR R_  (Rel name pk_v) = Table name (IR R_ pk_v)
--indexdfamily--     IR R_  (pk :~> v)      = IR Es_ pk ↦ IR Es_ v
--indexdfamily--     IR R_  (Set pk)        = IR Es_ pk ↦ Ø
--indexdfamily-- 
--indexdfamily-- 
--indexdfamily-- -- datatype with a tuple of relations
--indexdfamily-- class DtC a where
--indexdfamily--     dtInst :: a -> Inst (IR D_ a)
--indexdfamily--     instDt :: Inst (IR D_ a) -> Option a
--indexdfamily-- class RelsC a where
--indexdfamily--     relsInst :: a -> Inst (IR Rs_ a)
--indexdfamily--     instRels :: Inst (IR Rs_ a) -> Option a
--indexdfamily-- -- relation with a tuple of elements
--indexdfamily-- class RelC a where
--indexdfamily--     relInst :: a -> Inst (IR R_ a)
--indexdfamily--     instRel :: Inst (IR R_ a) -> Option a
--indexdfamily-- class EltsC a where
--indexdfamily--     eltsInst :: a -> Inst (IR Es_ a)
--indexdfamily--     instElts :: Inst (IR Es_ a) -> a
--indexdfamily-- -- element
--indexdfamily-- class EltC a where
--indexdfamily--     eltInst :: a -> Inst (IR E_ a)
--indexdfamily--     instElt :: Inst (IR E_ a) -> a
--indexdfamily-- 
--indexdfamily-- 
--indexdfamily-- instance RelsC rels => DtC (Dt name rels) where
--indexdfamily--     dtInst (Dt x) = relsInst x
--indexdfamily--     instDt x = Dt <$> instRels x
--indexdfamily-- -- relation-tuples
--indexdfamily-- instance (RelC a, RelsC (b,c,d)) => RelsC (a,b,c,d) where
--indexdfamily--     relsInst (a,b,c,d) = relInst a :& relsInst (b,c,d)
--indexdfamily--     instRels (x :& xs) = liftA2 (\a (b,c,d) -> (a,b,c,d)) (instRel x) (instRels xs)
--indexdfamily-- instance (RelC a, RelsC (b,c)) => RelsC (a,b,c) where
--indexdfamily--     relsInst (a,b,c) = relInst a :& relsInst (b,c)
--indexdfamily--     instRels (x :& xs) = liftA2 (\a (b,c) -> (a,b,c)) (instRel x) (instRels xs)
--indexdfamily-- instance (RelC a, RelC b) => RelsC (a, b) where
--indexdfamily--     relsInst (a,b) = relInst a :& relInst b :& TTablesEnd
--indexdfamily--     instRels (a :& b :& TTablesEnd) = liftA2 (,) (instRel a) (instRel b)
--indexdfamily-- -- singleton relation-tuples (same as "relation" below, but with the table-tuple list-wrapping)
--indexdfamily-- instance RelC pk_v => RelsC (Rel name pk_v) where
--indexdfamily--     relsInst (Rel x) = relInst x :& TTablesEnd
--indexdfamily--     instRels (x :& TTablesEnd) = Rel <$> instRel x
--indexdfamily-- -- TODO two more
--indexdfamily-- -- relation
--indexdfamily-- instance RelC pk_v => RelC (Rel name pk_v) where
--indexdfamily--     relInst (Rel x) = relInst x
--indexdfamily--     instRel x = Rel <$> instRel x
--indexdfamily-- instance (EltsC pk, EltsC v) => RelC (pk :~> v) where
--indexdfamily-- --  relInst
--indexdfamily-- --      = Map.map eltsInst
--indexdfamily-- --      . Map.mapKeys _2
--indexdfamily-- --  instRel
--indexdfamily-- --      = fmap Map.fromList
--indexdfamily-- --      . mapM (\(a, b) -> liftA2 (,) (convertFrom a) (convertFrom b))
--indexdfamily-- --      . Map.toList


--rawclasses-- -- | Your abstracted datatype must be an instance of this class.
--rawclasses-- class DtC a where
--rawclasses--     type DtIR a :: Type
--rawclasses--     dtInst :: a -> Inst (DtIR a)
--rawclasses--     instDt :: Inst (DtIR a) -> Option a
--rawclasses-- 
--rawclasses-- instance RelsC rels => DtC (Dt name rels) where
--rawclasses--     type DtIR (Dt name rels) = Schema name (RelsIR rels)
--rawclasses--     dtInst (Dt x) = relsInst x
--rawclasses--     instDt x = Dt <$> instRels x
--rawclasses-- 
--rawclasses-- ---- ------ -- Define a few instances for singleton-tuples that skip RelsC
--rawclasses-- ---- ------ instance RelC (Rel name pk_v) => DtC (Dt name (Rel name pk_v)) where
--rawclasses-- ---- ------     dtInst (Dt x) = relInst x
--rawclasses-- ---- ------  type DtIR (Dt name (Rel name pk_v)) = Schema name (RelIR (Rel name pk_v))
--rawclasses-- ---- ------ instance RelC (pk :~> v) => DtC (Dt name (pk :~> v)) where
--rawclasses-- ---- ------ instance RelC (Set pk) => DtC (Dt name (Set pk)) where
--rawclasses-- ---- ------     dtInst (Dt x) = _
--rawclasses-- ---- ------  instDt x = Dt <$> _
--rawclasses-- 
--rawclasses-- 
--rawclasses-- class RelsC a where
--rawclasses--     type RelsIR a :: Type
--rawclasses--     relsInst :: a -> Inst (RelsIR a)
--rawclasses--     instRels :: Inst (RelsIR a) -> Option a
--rawclasses-- 
--rawclasses-- ---- instance (RelC a, RelC b) => RelsC (a, b) where
--rawclasses-- ----     type RelsIR (a, b) = RelIR a & RelIR b & TablesEnd
--rawclasses-- ----     relsInst (a, b) = relInst a :& relInst b :& TTablesEnd
--rawclasses-- ----     instRels (a :& b :& TTablesEnd) =
--rawclasses-- ----         liftA2 (,) (instRel a) (instRel b)
--rawclasses-- ---- --instance (RelC a, RelC b, RelC c) => RelsC (a, b, c) where
--rawclasses-- ---- --    type RelsIR (a, b, c) = RelIR a & RelIR b & RelIR c & TablesEnd
--rawclasses-- ---- --    relsInst (a, b, c) = relInst a :& relInst b :& relInst c :& TTablesEnd
--rawclasses-- ---- --
--rawclasses-- ---- -- QQQ: Do we like wider classes or cons-style classes?
--rawclasses-- ---- instance (RelC a, RelsC (b, c)) => RelsC (a, b, c) where
--rawclasses-- ----     type RelsIR (a, b, c) = RelIR a & RelsIR (b, c)
--rawclasses-- ----     relsInst (a, b, c) = relInst a :& relsInst (b, c)
--rawclasses-- ----     instRels (a :& b_c) =
--rawclasses-- ----         liftA2 (\a (b,c) -> (a, b, c)) (instRel a) (instRels b_c)
--rawclasses-- ---- instance (RelC a, RelsC (b, c, d)) => RelsC (a, b, c, d) where
--rawclasses-- ----     type RelsIR (a, b, c, d) = RelIR a & RelsIR (b, c, d)
--rawclasses-- ----     relsInst (a, b, c, d) = relInst a :& relsInst (b, c, d)
--rawclasses-- ----     instRels (a :& b_c_d) =
--rawclasses-- ----         liftA2 (\a (b,c,d) -> (a, b, c, d)) (instRel a) (instRels b_c_d)
--rawclasses-- ---- -- TODO: larger tuples
--rawclasses-- 
--rawclasses-- 
--rawclasses-- --class RelC a where
--rawclasses-- --    type RelIR a :: Type
--rawclasses-- --    relInst :: a -> Inst (RelIR a)
--rawclasses-- --    instRel :: Inst (RelIR a) -> Option a
--rawclasses-- --
--rawclasses-- ----instance RelC (Rel name pk_v) where
--rawclasses-- ----    type RelIR (Rel name pk_v) = Table name (RelIR pk_v)
--rawclasses-- --instance (EltsC pk, EltsC v) => RelC (pk :~> v) where
--rawclasses-- --    type RelIR (pk :~> v) = EltsIR pk ↦ EltsIR v
--rawclasses-- --    relInst
--rawclasses-- --        = fmap eltsInst
--rawclasses-- --        . Map.mapKeys eltsInst
--rawclasses-- ---- instance RelC (Set pk) where
--rawclasses-- ----     type RelIR (Set pk) = EltsIR pk ↦ EltsIR ()
--rawclasses-- 
--rawclasses-- --- 
--rawclasses-- --- 
--rawclasses-- --- -- | Tag used by caller-classes so we don't deal with naked singletons
--rawclasses-- --- newtype Elts a = Elts a
--rawclasses-- 
--rawclasses-- class EltsC a where
--rawclasses--     type EltsIR a :: Type
--rawclasses--     eltsInst :: a -> Inst (EltsIR a)
--rawclasses--     instElts :: Inst (EltsIR a) -> a
--rawclasses-- 
--rawclasses-- --- -- instance EltsC (
--rawclasses-- 
--rawclasses-- -- 
--rawclasses-- -- instance Relations (Datatype name rs) where
--rawclasses-- --     type Schema (Datatype name rs) = Schema rs
--rawclasses-- 
--rawclasses-- -- instance Relations (a :@ index) where
--rawclasses-- 
--rawclasses-- -- -- FIXME: should be called "schema"
--rawclasses-- -- type family IR a :: Type where
--rawclasses-- --     IR (Datatype name rel_tuple) = Schema.Schema name (IR rel_tuple)
--rawclasses-- --     -- table-tuples
--rawclasses-- --     IR (Rel name pk_v) = Schema.Table name (Schema pk_v



class {-Relations (Abstracted a) =>-} MRDT a where
    type Abstracted a :: Type
    α :: a -> Abstracted a
    γ :: Abstracted a -> a


