{-# LANGUAGE RecordWildCards #-}

module Mog.MergeDriver.Merge where

import Control.Exception (Exception)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import System.FilePath (splitPath)
import qualified Data.Text as Text hiding (Text)

data MergeError
    = UnexpectedPathComponents FilePath
    deriving Show
instance Exception MergeError

data SchemaPath = SchemaPath
    { dtName :: Text
    , relName :: Text
--  , tupName :: Text
    , fieldName :: Text
    }

-- | @ordered-map/map/0c11d463c749db5838e2c0e489bf869d531e5403.tup/t1.val@
schemaPath :: FilePath -> Either MergeError SchemaPath
schemaPath fp =
    case Text.pack <$> splitPath fp of
        [dtName, relName, _tupName, fieldName] -> Right SchemaPath{..}
        _ -> Left $ UnexpectedPathComponents fp

type CBOR = ByteString
type BasedMerge = CBOR -> Merge
type Merge = CBOR -> CBOR -> Either MergeError CBOR

-- Use @mergedResultPathname@ to find a function in the IR type.
-- * Deserialize and do domain-specific three way merge on:
--     `cwd<>mergeAncestor`
--     `cwd<>currentVersion`
--     `cwd<>otherBranchVersion`
-- * Write the result in `cwd<>currentVersion`.
-- * Return success/failure over the socket.

-- | @ordered-map/map/0c11d463c749db5838e2c0e489bf869d531e5403.tup/t1.val@
class FindAndMerge s where
    merge :: Proxy s -> SchemaPath -> BasedMerge
