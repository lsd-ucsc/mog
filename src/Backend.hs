
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- | Git backend for MOG
module Backend where


import Codec.Serialise (Serialise, serialise)
-- import Crypto.Hash (hash, Digest, SHA1)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
-- import Data.Set (Set)
import Data.Type.Equality ((:~:)(..))

import GHC.TypeLits (KnownSymbol, symbolVal)
import Mog.Schema

-- import qualified Codec.Serialise as S
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Git as G
-- import qualified Output













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
