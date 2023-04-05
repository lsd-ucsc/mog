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

import Control.Arrow (first, second)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), throwE)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import Text.Read (readMaybe)
import Text.Regex.TDFA ((=~~))
import qualified Data.ByteString.Char8 as Char8 (pack)
import qualified Data.Text as Text (pack, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8')

import qualified Git

import qualified Mog.Output as Output

-- $setup
-- >>> :set -XOverloadedStrings




-- * Storage format to git blob/trees

data Field r
    = Atom  (Git.BlobOid r) Output.Role
    | Group (Git.TreeOid r)

-- TODO: consider how to store an Output.Database w/o erasing the other trees
-- (Output.Datatype) in the same database
--
-- TODO: storeDatabase :: Git.MonadGit r m => 

storeDatatype :: Git.MonadGit r m => Output.Datatype -> m (Git.TreeOid r)
storeDatatype
    -- TODO: We can control the merging of keys and values with suffixes:
    --   .gitattributes := ```
    --   *.pk merge=binary
    --   *.val merge=custom-driver
    --   *.blah merge=blah-driver # hypothetically we might want to give users the option to use existing merge drivers
    --   ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRelation)
    .   fmap (first $ Text.encodeUtf8 . Text.pack)
    .   snd -- FIXME: currently we just throw out the datatype name

-- | Create a tree OID containing, for each row, a hash of the key mapped to an
-- OID for the value. (Assumption: Key data is already in the value.)
storeRelation :: Git.MonadGit r m => [Output.Tuple] -> m (Git.TreeOid r)
storeRelation
    -- TODO: As an alternative to placing these configs at the datatype level,
    -- we can place them here by outputting the names of tuple columns:
    --   .gitattributes := ```
    --   */{pkfilecolumns} merge=binary
    --   */{valuecolumns} merge=custom-driver
    --   ```
    =   Git.createTree
    .   mapM (uncurry Git.putTree)
    <=< mapM (sequence . second storeRow)
    .   fmap (first $ Char8.pack . show) -- XXX: packing a hex-digest of a hash, so Char8.pack should be safe

-- | Create a tree OID containing, for each column, a tuple-index mapped to an OID.
storeRow :: Git.MonadGit r m => Output.Row -> m (Git.TreeOid r)
storeRow
    -- TODO: What if a row contains a foreign key? Those are subdirectories.
    -- Foreign keys are always referring to primary keys which don't merge
    -- (merge=binary).
    =   Git.createTree
    .   mapM (uncurry putCol)
    <=< return
    .   map addExt
    .   map (first $ Char8.pack . ('t':) . show) -- XXX: packing a single 't' followed by digits, so Char8.pack should be safe
    .   zip [0::Int ..]
    <=< mapM storeField
  where
    addExt (name, col) =
        let ext = case col of
                Atom  _bid Output.Pk  -> "pk"
                Atom  _bid Output.Val -> "val"
                Group _tid            -> "fk"
        in (name <> "." <> ext, col)

-- | Create an OID for one column.
storeField :: Git.MonadGit r m => Output.Field -> m (Field r)
storeField (Output.Atom bs role) = Atom <$> Git.createBlob (Git.BlobStringLazy bs) <*> return role
storeField (Output.Group row)    = Group <$> storeRow row

-- ** Store-pass utilities

-- | @putCol (path, column)@ puts the column value into the contextual-tree at the
-- specified path.
putCol :: Git.MonadGit r m => Git.TreeFilePath -> Field r -> Git.TreeT r m ()
putCol name col =
    case col of
        Atom  bid _role -> Git.putBlob name bid
        Group tid       -> Git.putTree name tid




-- * Git blob/trees to storage format

type Option = Either LoadError

data LoadError
    = WrongTreeEntryKindForExtension{kind::TreeEntryKind, extension::Text}
    | WrongIndex{gotIndex::Int, expectedIndex::Int}
    | InvalidFilename Text
    | UnicodeException UnicodeException
    deriving Show

loadField :: Field r -> m Output.Field
loadField (Atom  bid role) = undefined -- _2 -- TODO
loadField (Group tid)      = undefined -- _3 -- TODO

loadRowT :: Git.MonadGit r m => Git.TreeOid r -> ExceptT LoadError m Output.Row
loadRowT tid =
        mapM loadField
    <=< mapM (uncurry getCol)
    <=< mapM (\(ix, (n, e)) -> liftE $ (,) <$> matchIndexLoadExt ix n <*> return e)
    .   zip [0::Int ..]
    <=< lift
    $   Git.listTreeEntries
    <=< Git.lookupTree
    $   tid
  where
    liftE = ExceptT . pure

-- | Parse utf8 filename, match against expected naming format, match index
-- number, and return file extension.
--
-- >>> matchIndexLoadExt 0 "t0.pk"
-- Right "pk"
-- >>> matchIndexLoadExt 12 "t12.val"
-- Right "val"
--
-- >>> matchIndexLoadExt 0 "t.0.pk" -- multiple dots
-- Left (InvalidFilename "t.0.pk")
-- >>> matchIndexLoadExt 0 "t.pk" -- missing index
-- Left (InvalidFilename "t.pk")
-- >>> matchIndexLoadExt 0 ".pk" -- missing filename
-- Left (InvalidFilename ".pk")
-- >>> matchIndexLoadExt 0 "t0" -- missing extension
-- Left (InvalidFilename "t0")
-- >>> matchIndexLoadExt 0 "t0." -- empty extension
-- Left (InvalidFilename "t0.")
-- >>> matchIndexLoadExt 0 "t0o.pk" -- non-digit in index
-- Left (InvalidFilename "t0o.pk")
-- >>> matchIndexLoadExt 0 "t0.p$k" -- non-alnum in extension
-- Left (InvalidFilename "t0.p$k")
--
-- >>> matchIndexLoadExt 1 "t0.fk" -- wrong index
-- Left (WrongIndex {gotIndex = 0, expectedIndex = 1})
matchIndexLoadExt :: Int -> Git.TreeFilePath -> Option Text
matchIndexLoadExt index name = do
    txt <- bimap UnicodeException id $ Text.decodeUtf8' name
    (digits, ext) <- matchName txt
    i <- maybe (error "unreachable") pure . readMaybe $ Text.unpack digits
    if i == index
    then return ext
    else Left WrongIndex{gotIndex=i, expectedIndex=index}
  where
    namePattern = "^t([[:digit:]]+)\\.([[:alnum:]]+)$" :: Text
    matchNamePattern = (=~~ namePattern) :: Text -> Maybe (Text, Text, Text, [Text])
    matchName txt = case matchNamePattern txt of
        Just ("", t, "", [digits, ext]) | t == txt -> pure (digits, ext)
        _ -> Left $ InvalidFilename txt
-- TODO: differentiate error cases

-- ** Load-pass utilities

-- | @getCol fileExtension treeEntry@ converts the tree-entry to a column value
-- according to its file extension.
getCol :: Monad m => Text -> Git.TreeEntry r -> ExceptT LoadError m (Field r)
getCol "fk"  Git.TreeEntry{Git.treeEntryOid=tid} = return $ Group tid
getCol "pk"  Git.BlobEntry{Git.blobEntryOid=bid} = return $ Atom bid Output.Pk
getCol "val" Git.BlobEntry{Git.blobEntryOid=bid} = return $ Atom bid Output.Val
getCol ext treeEntry =
    throwE WrongTreeEntryKindForExtension{kind=treeEntryKind treeEntry, extension=ext}
-- TODO: consider ignoring file extensions; pass thru to atom, ignore for group
-- TODO: change Output.Role to a string/text

data TreeEntryKind
    = BlobEntry
    | TreeEntry
    | CommitEntry
    deriving Show

treeEntryKind :: Git.TreeEntry r -> TreeEntryKind
treeEntryKind Git.BlobEntry{} = BlobEntry
treeEntryKind Git.TreeEntry{} = TreeEntry
treeEntryKind Git.CommitEntry{} = CommitEntry
