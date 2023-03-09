module Output where

import Crypto.Hash ({-hash, -}Digest, SHA1)
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)

-- | Yes, it's recursive.
data Col
    = Prim ByteString
    | Ref [Col]
    deriving (Show, Eq, Ord)

-- | CBOR encoded columns representing a db-tuple.
type Row = [Col]

-- | A characteristic relation represented as a group of db-tuples.
--
--  * The key includes only the columns in the PK.
--  * The value includes all the columns.
type Table = Map Row Row

-- | A datatype represented as a group of named characteristic relations.
type Schema = Map String Table

-- | Multiple datatypes associated with their characteristic relations.
type Database = Map String Schema
