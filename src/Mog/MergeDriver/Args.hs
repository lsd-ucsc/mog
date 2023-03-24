{-# LANGUAGE DeriveGeneric #-}
module Mog.MergeDriver.Args where

import Codec.Serialise (Serialise)
import Options.Generic (Generic, ParseRecord)

data Args = Args
    { socketPath            :: FilePath -- ^ (ABSOLUTE PATH) To the Mog socket
    , mergeAncestor         :: FilePath -- ^ (CWD RELATIVE PATH) Git docs say: "merge ancestor's version (%O)"
    , currentVersion        :: FilePath -- ^ (CWD RELATIVE PATH) Git docs say: "current version (%A) ... leave the result of the merge in the file named with %A by overwriting it"
    , otherBranchVersion    :: FilePath -- ^ (CWD RELATIVE PATH) Git docs say: "other branches' version (%B)"
    , conflictMarkerSize    :: Int      -- ^ Git docs say: "%L will be replaced with the conflict marker size"
    , mergedResultPathname  :: FilePath -- ^ (REPO RELATIVE PATH) Git docs say: "the pathname in which the merged result will be stored via placeholder %P"
    }
    deriving (Generic, Show)

instance ParseRecord Args
instance Serialise Args
