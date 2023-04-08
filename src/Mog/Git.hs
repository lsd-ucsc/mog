{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module for interacting with a git backend to output/input a
-- 'Output.Database' value to/from disk
--
-- The functions in this module are polymorphic over the "gitlib" backend.
--
-- The naming convention in here is currently following that of the Output module.
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
import Text.Regex.TDFA ((=~~))
import qualified Data.Text as Text (pack, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8')

import qualified Git

import qualified Mog.Output as Output

-- $setup
-- >>> :set -XOverloadedStrings








-- * Storage format to git

-- TODO: consider how to store an Output.Database w/o erasing the other trees
-- (Output.Datatype) in the same database
--
-- TODO: storeDatabase :: Git.MonadGit r m => 




-- ** Helpers

-- | Parse utf8 filename. Return index and file extension.
--
-- >>> parseName "t0.pk"
-- Right (0,*.pk)
-- >>> parseName "t12.val"
-- Right (12,*.val)
--
-- >>> parseName "t0pk" -- no dot
-- Left (InvalidFilename "t0pk")
-- >>> parseName "t.0.pk" -- multiple dots
-- Left (InvalidFilename "t.0.pk")
-- >>> parseName "t.pk" -- missing index
-- Left (InvalidFilename "t.pk")
-- >>> parseName ".pk" -- missing name-part
-- Left (InvalidFilename ".pk")
-- >>> parseName "t0." -- empty extension-part
-- Left (InvalidFilename "t0.")
-- >>> parseName "t0o.pk" -- non-digit in index
-- Left (InvalidFilename "t0o.pk")
-- >>> parseName "t0.p$k" -- non-alnum in extension
-- Left (InvalidFilename "t0.p$k")
parseName :: Git.TreeFilePath -> Option (Int, FileExt)
parseName name = do
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

type Option = Either LoadError

storeDatatype :: Git.MonadGit r m => Output.Datatype -> m (Git.TreeOid r)
storeDatatype
    -- NOTE_GITATTRIBUTES: We can control the merging of keys and values with
    -- suffixes:
    --   .gitattributes := ```
    --   *.pk merge=binary
    --   *.val merge=custom-driver
    --   *.blah merge=blah-driver # hypothetically we might want to give users the option to use existing merge drivers
    --   ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRelation)
    .   fmap (first Text.encodeUtf8)
    .   snd -- FIXME: currently we just throw out the datatype name

-- | Create a tree OID containing, for each row, a hash of the key mapped to an
-- OID for the value. (Assumption: Key data is already in the value.)
storeRelation :: Git.MonadGit r m => [Output.Tuple] -> m (Git.TreeOid r)
storeRelation
    -- NOTE_GITATTRIBUTES: As an alternative to placing these configs at the
    -- datatype level, we can place them here by outputting the names of tuple
    -- columns:
    --   .gitattributes := ```
    --   */{pkfilecolumns} merge=binary
    --   */{valuecolumns} merge=custom-driver
    --   ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (\(hash, row) -> liftA2 (,) (storeHash hash) (storeRow row))
  where
    -- NOTE: Char8.pack might be safe, but it's better to use utf8 explicitly.
    storeHash = pure . Text.encodeUtf8 . Text.pack . show

loadRelation :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m [Output.Tuple]
loadRelation
    =   mapM (\(name, entry) -> liftA2 (,) (loadHash name) (loadEntryRow entry))
    <=< (lift . Git.listTreeEntries)
    <=< (lift . Git.lookupTree)
  where
    loadEntryRow (Git.TreeEntry tid) = loadRow tid
    loadEntryRow  Git.CommitEntry{}  = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry for a row in a relation; got a CommitEntry"}
    loadEntryRow  Git.BlobEntry{}    = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry for a row in a relation; got a BlobEntry"}
    loadHash name = do
        -- NOTE: Char8.unpack is unsafe for external data which may not be
        -- ascii, so we use Text.decodeUtf8' and surface a possible error.
        t <- either (throwE . UnicodeException) pure $ Text.decodeUtf8' name
        maybe (throwE $ InvalidHash t) pure . readMaybe $ Text.unpack t


storeRow :: Git.MonadGit r m => Output.Row -> m (Git.TreeOid r)
storeRow
    -- NOTE_GITATTRIBUTES: What if a row contains a foreign key? Those are
    -- subdirectories. Foreign keys are always referring to primary keys which
    -- don't merge (merge=binary).
    =   Git.createTree
    .   mapM (uncurry Git.putEntry)
    .   map mkName . zip [0::Int ..]
    <=< mapM storeCol
  where
    mkName (index, (FileExt ext, entry)) =
        let name = (Text.pack $ 't' : show index) <> "." <> ext in
        (Text.encodeUtf8 name, entry)

loadRow :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m Output.Row
loadRow
    =   mapM (uncurry loadCol)
    <=< mapM (uncurry unName) . zip [0::Int ..]
    <=< (lift . Git.listTreeEntries)
    <=< (lift . Git.lookupTree)
  where
    liftE = ExceptT . pure
    unName index (path, entry) = do
        (i, ext) <- liftE $ parseName path
        if i == index
        then return (ext, entry)
        else throwE WrongIndex{gotIndex=i, expectedIndex=index}


-- | Write-out serialized column data to create an OID and choose a file name
-- extension. Atom's use their tag, groups use a made-up extension.
storeCol :: Git.MonadGit r m => Output.Col -> m (FileExt, Git.TreeEntry r)
storeCol col = liftA2 (,) (pure $ ext col) (store col)
  where
    ext (Output.Group _)    = FileExt "fk"
    ext (Output.Atom _ tag) = FileExt tag
    store (Output.Group row) = Git.TreeEntry <$> storeRow row
    store (Output.Atom bs _) = Git.BlobEntry <$> Git.createBlob (Git.BlobStringLazy bs) <*> return Git.PlainBlob

-- | Read-in serialized column data from its OID and store the given extension
-- if the column is an Atom.
loadCol :: Git.MonadGit r m => FileExt -> Git.TreeEntry r -> ExceptT LoadError m Output.Col
loadCol _             (Git.TreeEntry tid)   = Output.Group <$> loadRow tid
loadCol (FileExt ext) (Git.BlobEntry bid _) = Output.Atom <$> lift (Git.catBlobLazy bid) <*> return ext
loadCol _              Git.CommitEntry{}    = throwE UnexpectedTreeEntry{reason="expecting a TreeEntry (Group) or BlobEntry (Atom) for a column in a row; got a CommitEntry"}
