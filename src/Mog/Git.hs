{-# LANGUAGE OverloadedStrings #-}

-- | Module for interacting with git by reading or writing the storage format.
-- Functions in this module are polymorphic over the "gitlib" backend.
module Mog.Git where

import Control.Applicative (liftA2)
import Control.Arrow (first, second)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Text.Read (readMaybe)
import qualified Data.Text as Text (pack, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8')

import Text.Regex.TDFA ((=~~))
import qualified Git

import qualified Mog.Output as Output

-- $setup
-- >>> :set -XOverloadedStrings




-- * Storage format to git


-- ** Helpers

times :: Applicative f => (f a, f b) -> f (a, b)
times = uncurry (liftA2 (,))
{-# INLINE times #-}

parallel :: Applicative m => (a -> m b) -> (x -> m y) -> (a, x) -> m (b, y)
parallel f g = times . bimap f g
{-# INLINE parallel #-}

-- | Parse utf8 filename. Return index and file extension.
--
-- >>> parseFieldName "t0.pk"
-- Right (0,*.pk)
-- >>> parseFieldName "t12.val"
-- Right (12,*.val)
--
-- >>> parseFieldName "t0pk" -- no dot
-- Left (InvalidFilename "t0pk")
-- >>> parseFieldName "t.0.pk" -- multiple dots
-- Left (InvalidFilename "t.0.pk")
-- >>> parseFieldName "t.pk" -- missing index
-- Left (InvalidFilename "t.pk")
-- >>> parseFieldName ".pk" -- missing name-part
-- Left (InvalidFilename ".pk")
-- >>> parseFieldName "t0." -- empty extension-part
-- Left (InvalidFilename "t0.")
-- >>> parseFieldName "t0o.pk" -- non-digit in index
-- Left (InvalidFilename "t0o.pk")
-- >>> parseFieldName "t0.p$k" -- non-alnum in extension
-- Left (InvalidFilename "t0.p$k")
parseFieldName :: Git.TreeFilePath -> Either LoadError (Int, FileExt)
parseFieldName name = do
    t <- bimap UnicodeException id $ Text.decodeUtf8' name
    (digits, ext) <- matchName t
    ix <- maybe (error "bug in regex") pure . readMaybe $ Text.unpack digits
    return (ix, FileExt ext)
  where
    namePattern = "^t([[:digit:]]+)\\.([[:alnum:]]+)$" :: Text
    matchNamePattern = (=~~ namePattern) :: Text -> Maybe (Text, Text, Text, [Text])
    matchName txt = case matchNamePattern txt of
        Just ("", t, "", [digits, ext]) | t == txt -> pure (digits, ext)
        _ -> Left $ InvalidFilename txt
-- TODO: differentiate error cases




-- ** Functions to implement conversion

newtype FileExt = FileExt Text
instance Show FileExt where show (FileExt ext) = '*':'.':Text.unpack ext

data LoadError
    = InvalidHash Text
    | UnexpectedTreeEntry{reason::String}
    | WrongIndex{gotIndex::Int, expectedIndex::Int}
    | InvalidFilename Text
    | UnicodeException UnicodeException
    deriving Show

-- FIXME: we want to be able to store and load datatypes independenty of each
-- other, but this is a start
storeDatabase :: Git.MonadGit r m => [Output.Datatype] -> m (Git.TreeOid r)
storeDatabase
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (parallel (return . Text.encodeUtf8) storeRelations)

-- FIXME: we want to be able to store and load datatypes independenty of each
-- other, but this is a start
loadDatabase :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m [Output.Datatype]
loadDatabase
    =   mapM (parallel loadName loadEntryRelations)
    <=< (lift . Git.listTreeEntries)
    <=< (lift . Git.lookupTree)
   where
    loadEntryRelations (Git.TreeEntry tid) = loadRelations tid
    loadEntryRelations  Git.CommitEntry{}  = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry of relations in a datatype; got a CommitEntry"}
    loadEntryRelations  Git.BlobEntry{}    = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry of relations in a datatype; got a BlobEntry"}
    loadName name = either (throwE . UnicodeException) pure $ Text.decodeUtf8' name


storeRelations :: Git.MonadGit r m => [Output.Relation] -> m (Git.TreeOid r)
storeRelations
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (parallel (return . Text.encodeUtf8) storeTuples)

loadRelations :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m [Output.Relation]
loadRelations
    =   mapM (parallel loadName loadEntryTuples)
    <=< (lift . Git.listTreeEntries)
    <=< (lift . Git.lookupTree)
   where
    loadEntryTuples (Git.TreeEntry tid) = loadTuples tid
    loadEntryTuples  Git.CommitEntry{}  = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry of tuples in a relation; got a CommitEntry"}
    loadEntryTuples  Git.BlobEntry{}    = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry of tuples in a relation; got a BlobEntry"}
    loadName name = either (throwE . UnicodeException) pure $ Text.decodeUtf8' name


storeTuples :: Git.MonadGit r m => [Output.Tuple] -> m (Git.TreeOid r)
storeTuples
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (parallel storeHash storeRow)
  where
    -- NOTE: Char8.pack might be safe, but it's better to use utf8 explicitly.
    storeHash = pure . Text.encodeUtf8 . Text.pack . show

loadTuples :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m [Output.Tuple]
loadTuples
    =   mapM (parallel loadHash loadEntryRow)
    <=< (lift . Git.listTreeEntries)
    <=< (lift . Git.lookupTree)
  where
    loadEntryRow (Git.TreeEntry tid) = loadRow tid
    loadEntryRow  Git.CommitEntry{}  = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry of fields in a tuple; got a CommitEntry"}
    loadEntryRow  Git.BlobEntry{}    = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry of fields in a tuple; got a BlobEntry"}
    loadHash name = do
        -- NOTE: Char8.unpack is unsafe for external data which may not be
        -- ascii, so we use Text.decodeUtf8' and surface a possible error.
        t <- either (throwE . UnicodeException) pure $ Text.decodeUtf8' name
        maybe (throwE $ InvalidHash t) pure . readMaybe $ Text.unpack t


storeRow :: Git.MonadGit r m => [Output.Field] -> m (Git.TreeOid r)
storeRow
    =   Git.createTree
    .   mapM (uncurry Git.putEntry)
    .   map mkName . zip [0::Int ..]
    <=< mapM storeField
  where
    mkName (index, (FileExt ext, entry)) =
        let name = (Text.pack $ 't' : show index) <> "." <> ext in
        (Text.encodeUtf8 name, entry)

loadRow :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m [Output.Field]
loadRow
    =   mapM (uncurry loadField)
    <=< mapM (uncurry unName) . zip [0::Int ..]
    <=< (lift . Git.listTreeEntries)
    <=< (lift . Git.lookupTree)
  where
    liftE = ExceptT . pure
    unName index (path, entry) = do
        (i, ext) <- liftE $ parseFieldName path
        if i == index
        then return (ext, entry)
        else throwE WrongIndex{gotIndex=i, expectedIndex=index}


-- | Write-out serialized field data to create an OID and choose a filename
-- extension. Atoms use their tag, and groups use a made-up extension.
storeField :: Git.MonadGit r m => Output.Field -> m (FileExt, Git.TreeEntry r)
storeField field = parallel (pure . ext) store (field, field)
  where
    ext (Output.Group _)    = FileExt "fk"
    ext (Output.Atom _ tag) = FileExt tag
    store (Output.Group row) = Git.TreeEntry <$> storeRow row
    store (Output.Atom bs _) = Git.BlobEntry <$> Git.createBlob (Git.BlobStringLazy bs) <*> return Git.PlainBlob

-- | Read-in serialized field data from an OID and store the given extension if
-- the field is an Atom.
loadField :: Git.MonadGit r m => FileExt -> Git.TreeEntry r -> ExceptT LoadError m Output.Field
loadField _             (Git.TreeEntry tid)   = Output.Group <$> loadRow tid
loadField (FileExt ext) (Git.BlobEntry bid _) = Output.Atom <$> lift (Git.catBlobLazy bid) <*> return ext
loadField _              Git.CommitEntry{}    = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry (Group) or BlobEntry (Atom) for a Fieldumn in a row; got a CommitEntry"}
