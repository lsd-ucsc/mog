{-# OPTIONS_GHC "-Wno-missing-signatures" #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Git backend for MOG
module Backend where

-- import Control.Monad.IO.Class (MonadIO(..))
-- import Control.Applicative (liftA2)
-- import Data.ByteString.Char8 (pack)
import Data.Proxy (Proxy(..))

import GHC.TypeLits (KnownSymbol, symbolVal)

import Data.Map (Map)
import qualified Data.Map as Map

-- import Data.Set (Set)
import qualified Data.Set as Set

-- import Crypto.Hash (hash, Digest, SHA1)
import Codec.Serialise (Serialise, serialise)
-- import qualified Codec.Serialise as S

import GHC.TypeLits (Symbol)
import Data.Type.Equality ((:~:)(..))

-- import qualified Git as G
import Terms
import qualified Output

--- -- Column
--- --  Type
--- --      Data-type or (Index of a previous table)
--- --  Value
--- --      Data-value or (Value of Pk type from the previous table)
--- {-
--- data Col : Type_1 where
---     Prim : Type → Col
---     Ref : Index → Col
--- 
--- data Field : (c : Col) → Type where
---     prim : ∀{T} → T → Field (Prim T)
---     ref : ∀{ix} → RawFilePath → Field (Ref ix)
--- -}
--- 
--- -- Row is a het-list of col.
--- {-
--- data Row : Type_1 where
---     (:%:) : Col → Row → Row
---     () : Row
--- -}

data Schema (name :: Symbol) tables -- ^ Table-group wrapper
data t & ts -- ^ Table-group cons
infixr 5 &
data TablesEnd -- ^ Table-group nil

data Table (name :: Symbol) pk_v -- ^ Table (one) wrapper
data pk ↦ v -- ^ Table (one) spec
infix 6 ↦

data c % cs -- ^ Tuple cons
infixr 7 %
data Ø -- ^ Tuple nil

data Index = Here | There Index
data Ref fk (index :: Index)
data Prim a

-- This isn't a correct use of these symbols; this test only looks at precedence.
_testPrec10 = Refl :: Int & Char % Word % () ↦ String % Float % () & Double
            :~: Int & (((Char % (Word % ())) ↦ (String % (Float % ()))) & Double)

---- TODO: instances to constrain to valid schemas
----
---- -- A fk must match the pk of the table indicated by the index.
---- class                          Fk fk (index :: Index) tables                             where
---- instance Fk fk index tables => Fk fk (There index)    (t :#: tables)                     where -- inductive case
---- instance                       Fk pk  Here            (PkTable name pk tuple :#: tables) where -- base case
---- 
---- -- All fks must be valid. This is just (Map (\fk index -> ForeignKey fk index tables) tuple) that skips non-reference types.
---- class                                              Fks tuple                    tables where
---- instance (Fks tuple tables, Fk fk index tables) => Fks (Ref fk index :%: tuple) tables where -- overlapping inductive case
---- instance  Fks tuple tables                      => Fks (a            :%: tuple) tables where -- overlappable inductive case
---- instance                                           Fks ()                       tables where -- base case
---- 
---- -- A table has a valid pk and fks.
---- class                                       Tbl table                   tables where
---- instance (Pk pk tuple, Fks tuple tables) => Tbl (PkTable name pk tuple) tables where

---- type family as %++ bs :: * where
----     ()        %++ bs = bs
----     (a :% as) %++ bs = a :% (as %++ bs)

type family Col c :: * where
    Col (Prim a)   = a
    Col (Ref fk _) = Cols fk

-- | Convert a tuple schema to an instance of that tuple (using a hetlist).
type family Cols c :: * where
    Cols Ø        = TTupleEnd
    Cols (c % cs) = Col c :% Cols cs

_testCols10 = Refl :: Cols (Prim Char % Ø)
            :~: (Char :% TTupleEnd)
_testCols20 = Refl :: Cols (Ref (Prim Char % Ø) 'Here % Ø)
            :~: ((Char :% TTupleEnd) :% TTupleEnd)
_testCols30 = Refl :: Cols (Prim Int % Ref (Prim Char % Prim Word % Ø) 'Here % Prim Double % Ø)
            :~: Int :% (Char :% Word :% TTupleEnd) :% Double :% TTupleEnd
_testCols31 = Refl :: Cols (Prim Int % Ref (Prim Char % Prim Word % Ø) 'Here % Prim Double % Ø)
            :~: Int :% (Char :% Word :% TTupleEnd) :% Double :% TTupleEnd

-- | Compute the type of an instance of a database schema.
type family Inst a :: * where
    Inst (Schema name ts)  = Inst ts
    Inst (t & ts)          = Inst t :& Inst ts
    Inst TablesEnd         = TTablesEnd
    Inst (Table name pk_v) = Inst pk_v
    Inst (pk ↦ v)          = Map (Cols pk) (Cols v)

_testInst10 = Refl :: Inst (Prim Int % Ø ↦ Prim String % Ø)
            :~: Map (Int :% TTupleEnd) (String :% TTupleEnd)
_testInst20 = Refl :: Inst (Table "teble" (Prim Int % Ø ↦ Prim String % Ø))
            :~: Map (Int :% TTupleEnd) (String :% TTupleEnd)

type PairSchema a =
    Schema "pair"
    ( Table "map" (Prim Bool % Ø ↦ Prim a % Ø)
    & TablesEnd
    )
testInst25 = Refl :: Inst (PairSchema a)
            :~: Map (Bool :% TTupleEnd) (a :% TTupleEnd) :& TTablesEnd

type TwopleSchema a b =
    Schema "twople"
    ( Table "singleton" (Ø ↦ Prim a % Prim b % Ø)
    & TablesEnd
    )
testInst26 = Refl :: Inst (TwopleSchema a b)
            :~: Map TTupleEnd (a :% b :% TTupleEnd) :& TTablesEnd

type QueueSchema a =
    Schema "queue"
    ( Table "ob" (Ref (Prim a % Ø) 'Here % Ref (Prim a % Ø) 'Here % Ø ↦ Ø)
    & Table "mem" (Prim a % Ø ↦ Ø)
    & TablesEnd
    )
_testInst30 = Refl :: Inst (QueueSchema a)
            :~: Map ((a :% TTupleEnd) :% (a :% TTupleEnd) :% TTupleEnd) TTupleEnd
            :& Map (a :% TTupleEnd) TTupleEnd
            :& TTablesEnd

queueExample :: Inst (QueueSchema Int)
queueExample =
        fromList [((4 :% TTupleEnd) :% (2 :% TTupleEnd) :% TTupleEnd), ((5 :% TTupleEnd) :% (4 :% TTupleEnd) :% TTupleEnd)]
    :&  fromList [2 :% TTupleEnd, 4 :% TTupleEnd, 5 :% TTupleEnd]
    :&  TTablesEnd
  where
    fromList :: Ord a => [a] -> Map a TTupleEnd
    fromList = Map.fromSet (const TTupleEnd) . Set.fromList

type OrderedMapSchema k v =
    Schema "ordered-map"
    ( Table "ord" (Ref (Prim k % Ø) ('There 'Here) % Ref (Prim k % Ø) ('There 'Here) % Ø ↦ Ø)
    & Table "map" (Ref (Prim k % Ø) 'Here % Ø ↦ Prim v % Ø)
    & Table "keys" (Prim k % Ø ↦ Ø)
    & TablesEnd
    )
_testInst40 = Refl :: Inst (OrderedMapSchema k v)
            :~: Map ((k :% TTupleEnd) :% (k :% TTupleEnd) :% TTupleEnd) TTupleEnd
            :& Map ((k :% TTupleEnd) :% TTupleEnd) (v :% TTupleEnd)
            :& Map (k :% TTupleEnd) TTupleEnd
            :& TTablesEnd

class Out a where
    type Output a :: *
    out :: Proxy a -> Inst a -> Output a

-- Yield an anonymous 'Output.Schema' for an instance of a datatype schema.
-- Except currently we don't do that; we return a hetlist of table pairs.
-- TODO: somewhere we need to wrap it with a map
instance (Out ts, KnownSymbol name) => Out (Schema name ts) where
    type Output (Schema name ts) = (String, Output ts)
    out Proxy x = (symbolVal @name Proxy, out @ts Proxy x)

-- TODO: think about whether this would work later; the main question is
-- whether the equality constraint works; also there's some weirdness with
-- unioning the maps
---- instance (Out t, Out ts, Output t ~ Output ts) => Out (t & ts) where
----     type Output (t & ts) = Map String () -- ...  (Output t) ... (Output ts)
----     out Proxy (t :& ts) = _

-- Map 'out' over each table.
--
-- TODO: you need to put them in a 'Map' later.
instance (Out t, Out ts) => Out (t & ts) where
    type Output (t & ts) = Output t :& Output ts
    out Proxy (x :& xs) = out @t Proxy x :& out @ts Proxy xs

instance Out TablesEnd where
    type Output TablesEnd = TTablesEnd
    out Proxy TTablesEnd = TTablesEnd

-- Process the table and return a tuple containing its name.
instance (Out pk_v, KnownSymbol name) => Out (Table name pk_v) where
    type Output (Table name pk_v) = (String, Output pk_v)
    out Proxy x = (symbolVal @name Proxy, out @pk_v Proxy x)

instance (OutCols pk, OutCols v) => Out (pk ↦ v) where
    type Output (pk ↦ v) = Map Output.Row Output.Row
    out Proxy
        = Map.mapWithKey (\k -> mappend k . outCols @v Proxy)
        . Map.mapKeys (outCols @pk Proxy)

class OutCols a where
    outCols :: Proxy a -> Cols a -> Output.Row

instance (OutCol c, OutCols cs) => OutCols (c % cs) where
    outCols Proxy (c :% cs) = outCol @c Proxy c : outCols @cs Proxy cs

instance OutCols Ø where
    outCols Proxy TTupleEnd = []

class OutCol a where
    outCol :: Proxy a -> Col a -> Output.Col

instance Serialise a => OutCol (Prim a) where
    outCol Proxy = Output.Prim . serialise

instance OutCols fk => OutCol (Ref fk index) where
    outCol Proxy = Output.Ref . outCols @fk Proxy

_testOutput10 = Refl :: Output (QueueSchema a)
            :~: ( String
                ,    (String, Map Output.Row Output.Row)
                  :& (String, Map Output.Row Output.Row)
                  :& TTablesEnd
                )










-- ---- -- TODO: ordered-map schema with three tables
-- ---- 
-- ---- --class                                             Schema a                          where type Inst a :: *
-- ---- --instance                                          Schema (Database name ())         where type Inst (Database name ()) = ()
-- ---- --instance (Schema t, Schema (Database name ts)) => Schema (Database name (t :#: ts)) where type Inst (Database name (t :#: ts)) = Inst t ::: Inst (Database name ts)
-- ---- 
-- ---- --type family Tuple a :: * where
-- ---- --    Tuple () = ()
-- ---- --    Tuple (a :%: ()) = a
-- ---- --    Tuple (a :%: b :%: ()) = (a, b)
-- ---- --    Tuple (a :%: b :%: c :%: ()) = (a, b, c)
-- ---- 
-- ---- 
-- ---- 
-- ---- -- -- Compute the type of instances of a column schema.
-- ---- 
-- ---- 
-- ---- -- _test01 :: Cols (Prim Int :%: Ref (Prim String :%: ()) Here :%: ())
-- ---- --         :~: Inst (Int :%: String :%: ())
-- ---- -- _test01 = Refl
-- ---- 
-- ---- 
-- ---- 
-- ---- -- -- instance (Output t, Output ts) => Output (t :#: ts) where
-- ---- -- --     type Outputted (t :#: ts) = Output.Schema
-- ---- -- 
-- ---- -- 
-- ---- -- -- Table (A characteristic relation of a datatype)
-- ---- -- --  Type
-- ---- -- --      Cols: Sequence of column-types
-- ---- -- --      Pk: Optional subsequence of column-types
-- ---- -- --  Value
-- ---- -- --      Set of (Sequence of column-values)
-- ---- -- {-
-- ---- -- record Schema : Type_1 where
-- ---- --     field key : Row
-- ---- --     field val : Row
-- ---- -- -}
-- ---- -- 
-- ---- -- -- data Ref fk (index :: Index) = Ref fk deriving (Eq, Ord)
-- ---- -- -- -- 1. Check the `a` equals the PK of the table at `index`.
-- ---- -- -- -- 3. Use the `a` value to ensure the referenced tuple exists. (Do we even have PK lookup?)
-- ---- -- -- -- 2. Use the `a` value to compute the path to store in the reference.
-- ---- -- -- 
-- ---- -- -- -- Database (All characteristic relations of a datatype; an MRDT)
-- ---- -- -- --  Type
-- ---- -- -- --      Sequence of table-types
-- ---- -- -- --  Value
-- ---- -- -- --      Sequence of table-values
-- ---- -- -- newtype Database tables = Database tables
-- ---- -- -- data a :#: b = a :#: b
-- ---- -- -- infixr :#:
-- ---- -- -- -- Serialize in the git monad.
-- ---- -- -- class Ser r a where
-- ---- -- --     type Serred r a :: *
-- ---- -- --     ser :: G.MonadGit r m => a -> m (Serred r a)
-- ---- -- -- 
-- ---- -- -- -- Serialize (with two arguments) in the git monad.
-- ---- -- -- class Ser2 r a b where
-- ---- -- --     type Serred2 r a b :: *
-- ---- -- --     ser2 :: G.MonadGit r m => a -> b -> m (Serred2 r a b)
-- ---- -- -- 
-- ---- -- -- -- Serialize in the tree monad.
-- ---- -- -- class SerT r a where
-- ---- -- --     type SerredT r a :: *
-- ---- -- --     serT :: G.MonadGit r m => a -> G.TreeT r m (SerredT r a)
-- ---- -- -- 
-- ---- -- -- -- -- instance Ser r (Database tables) => Ser r (Database (table :#: tables)) where
-- ---- -- -- -- -- Consider maintaining the tag?
-- ---- -- -- -- instance Ser r tables => Ser r (Database tables) where
-- ---- -- -- 
-- ---- -- -- -- Wrapper for some type aliases so we can make typeclass instances with them
-- ---- -- -- newtype BID r = BID (G.BlobOid r)
-- ---- -- -- newtype TID r = TID (G.TreeOid r)
-- ---- -- -- 
-- ---- -- -- -- instance (Ser2 r table tables, Ser r tables) => Ser r (table :#: tables) where
-- ---- -- -- 
-- ---- -- -- ---- First pass:
-- ---- -- -- ---- (1) Convert each table to something.
-- ---- -- -- --instance (Ser2 r table tables, Ser r tables) => Ser r (table :#: tables) where
-- ---- -- -- --    type Serred r (table :#: tables) = Serred2 r table tables :#: Serred r tables
-- ---- -- -- --    ser (table :#: tables) = liftA2 (:#:) (ser2 table tables) (ser tables)
-- ---- -- -- ---- (1.1) Convert tables to TIDs.
-- ---- -- -- --instance Ser2 r (PkTable name pk tuple) tables where
-- ---- -- -- --    type Serred2 r (PkTable name pk tuple) tables = TID r
-- ---- -- -- --    ser2 (PkTable xs) tables = do
-- ---- -- -- --        undefined
-- ---- -- -- ---- Second pass:
-- ---- -- -- ---- (2) Reduce TIDs to a TID.
-- ---- -- -- --instance Ser r (TID r :#: tids) where
-- ---- -- -- --
-- ---- -- -- --
-- ---- -- -- -- Enter DB
-- ---- -- -- --  For each table
-- ---- -- -- --      Enter table
-- ---- -- -- 
-- ---- -- -- class SerTable r table tables where
-- ---- -- --     serTable :: (G.MonadGit r m, Tbl table tables) => table -> tables -> m (G.TreeOid r)
-- ---- -- -- 
-- ---- -- -- instance (SerTuple r tuple tables) => SerTable r (PkTable name pk tuple) tables where
-- ---- -- --     serTable (PkTable xs) tables = undefined
-- ---- -- -- 
-- ---- -- -- -- class SerTuple r tuple tables where
-- ---- -- -- --     serTuple :: (G.MonadGit r m, Pk pk tuple => tuple -> tables -> m (G.BlobOid r)
-- ---- -- -- 
-- ---- -- -- 
-- ---- -- -- 
-- ---- -- -- class                                     MakeBlob r a         where type Blobbed r a :: *                             ; makeBlob :: G.MonadGit r m => a -> m (Blobbed r a)
-- ---- -- -- instance                                  MakeBlob r ()        where type Blobbed r () = ()                            ; makeBlob () = pure ()
-- ---- -- -- instance (S.Serialise a, MakeBlob r b) => MakeBlob r (a :%: b) where type Blobbed r (a :%: b) = (BID r :%: Blobbed r b); makeBlob (a :%: b) = liftA2 (:%:) (fmap BID . G.createBlob . G.BlobStringLazy . S.serialise $ a) (makeBlob b)
-- ---- -- -- 
-- ---- -- -- class                     TupleLength a         where tupleLength :: a -> Word
-- ---- -- -- instance TupleLength b => TupleLength (a :%: b) where tupleLength (_ :%: b) = 1 + tupleLength b
-- ---- -- -- instance                  TupleLength ()        where tupleLength () = 0
-- ---- -- -- 
-- ---- -- -- -- Tuples are reverse-indexed, so in a 3 element tuple the first element is `t2`, the next is `t1`, and the last is always `t0`
-- ---- -- -- tupleIndex :: TupleLength a => a -> G.TreeFilePath
-- ---- -- -- tupleIndex = pack . ('t':) . show . tupleLength
-- ---- -- -- 
-- ---- -- -- class                                     MakeTree r a             where type Treed r a :: *                            ; makeTree :: G.MonadGit r m => a -> G.TreeT r m (Treed r a)
-- ---- -- -- instance (TupleLength b, MakeTree r b) => MakeTree r (BID r :%: b) where type Treed r (BID r :%: b) = (() :%: Treed r b); makeTree ((BID a) :%: b) = liftA2 (:%:) (G.putBlob (tupleIndex b) a) (makeTree b)
-- ---- -- -- instance                                  MakeTree r ()            where type Treed r () = ()                           ; makeTree () = pure ()
-- ---- -- -- 
-- ---- -- -- -- instance MakeTree r tuple => MakeTree r (PkTable pk tuple) where
-- ---- -- -- --     type Treed r (PkTable pk tuple) = ()
-- ---- -- -- --     makeTree (PkTable xs) = mapM_ ( xs
-- ---- -- -- -- -- Wrapper for tree ids so we can make typeclass instances with them
-- ---- -- -- 
-- ---- -- -- class Len a where len :: a -> Word
-- ---- -- -- 
-- ---- -- -- lenName :: Len a => Char -> a -> G.TreeFilePath
-- ---- -- -- lenName c = pack . (c:) . show . len
-- ---- -- -- 
-- ---- -- -- instance          Len ()        where len ()        = 0
-- ---- -- -- instance Len b => Len (a :#: b) where len (_ :#: b) = 1 + len b
-- ---- -- -- instance Len b => Len (a :%: b) where len (_ :%: b) = 1 + len b
-- ---- -- -- -- FIXME: using the same function for both delimiters allows malformed lists
-- ---- -- -- -- using either delimiter
-- ---- -- -- 
-- ---- -- -- -- XXX: everywhere we might call "create tree" and return an OID, it might
-- ---- -- -- -- behoove us to think ahead and return the name, no?
-- ---- -- -- 
-- ---- -- -- -- TODO: constrain pk to a subset of tuple
-- ---- -- -- -- TODO: constrain refs in tuple to be valid against tables
-- ---- -- -- -- instance (SerT r tuple, Ser r tables, Len tables)
-- ---- -- -- --     => Ser r (PkTable pk tuple :#: tables) where
-- ---- -- -- --     type Serd r (PkTable pk tuple :#: tables) = (G.TreeOid r :#: Serd r tables)
-- ---- -- -- --     ser (PkTable xs :#: tables) = do
-- ---- -- -- --         -- FIXME: instead of createTree, use withNewTree and *return the hash value*
-- ---- -- -- --         ys <- mapM (G.createTree . serT) . Set.toList $ xs
-- ---- -- -- --         t <- G.createTree $ do
-- ---- -- -- --             -- in here we need to create the relation's tree by `putTree path
-- ---- -- -- --             undefined
-- ---- -- -- --         -- We can't actually name this relation here b/c we don't have a reference to the current tree
-- ---- -- -- --         liftA2 (:#:) undefined (ser tables)
-- ---- -- -- 
-- ---- -- -- -- ghci> import Data.ByteArray (convert)
-- ---- -- -- -- ghci> import Crypto.Hash (hash, Digest, SHA1)
-- ---- -- -- -- ghci> import Data.ByteString (ByteString)
-- ---- -- -- -- ghci> :set -XOverloadedStrings
-- ---- -- -- -- ghci> (hash ("blob 6\0ehello" :: ByteString) :: Digest SHA1)
-- ---- -- -- -- 176eec6944336ace0e18492da7538825e8c5ac8e
-- ---- -- -- -- ghci> convert (hash ("blob 6\0ehello" :: ByteString) :: Digest SHA1) :: ByteString
-- ---- -- -- -- ...
-- ---- -- -- -- Convert might be the wrong thing to do in this case
-- ---- -- -- 
-- ---- -- -- 
-- ---- -- -- newtype TupleOid r = TupleOid (G.TreeOid r)
-- ---- -- -- 
-- ---- -- -- make3Tuple :: (G.MonadGit r m, S.Serialise a, S.Serialise b, S.Serialise c) =>
-- ---- -- --     (a, b, c) -> m (TupleOid r)
-- ---- -- -- make3Tuple (x1, x2, x3) = do
-- ---- -- --     b1 <- G.createBlob . G.BlobStringLazy . S.serialise $ x1
-- ---- -- --     b2 <- G.createBlob . G.BlobStringLazy . S.serialise $ x2
-- ---- -- --     b3 <- G.createBlob . G.BlobStringLazy . S.serialise $ x3
-- ---- -- --     tree <- G.createTree $ do
-- ---- -- --         G.putBlob "t1" b1
-- ---- -- --         G.putBlob "t2" b2
-- ---- -- --         G.putBlob "t3" b3
-- ---- -- --     return $ TupleOid tree
