{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}

-- | Module to implement a pidfile or other mutual exclusion trick to prevent
-- concurrent access to a repo.
module Mog.Git.Mutex
    ( withPidSymlink
    , PidSymlinkError(..)
    ) where

import Control.Exception (Exception, catch, bracket_, throwIO)
import System.Directory (createFileLink, removeFile, getSymbolicLinkTarget)
import System.IO (stderr, hPutStrLn)
import System.IO.Error (isDoesNotExistError, isAlreadyExistsError)
import System.Posix (getProcessID)

data PidSymlinkError
    = PidSymlinkPathAlreadyExists{exc::IOError}
    | PidSymlinkRaceLost{winner::FilePath}
    | PidSymlinkMissingOnCleanup{exc::IOError} -- ^ Never thrown; only logged. No point in returning an error if the action's result is available.
    deriving Show

-- | Not for export
newtype Wrapped = Wrapped PidSymlinkError deriving Show
instance Exception Wrapped

throwWrapped :: PidSymlinkError -> IO a
throwWrapped = throwIO . Wrapped

-- | Create a pid-symlink at the specified path or return an error. Otherwise
-- execute the given action. If the action is executed, its result is returned.
withPidSymlink :: FilePath -> IO a -> IO (Either PidSymlinkError a)
withPidSymlink path action = do
    pid <- getProcessID -- 'unix' package
    bracket_ (acquire pid) release (inner pid)
        `catch`
        \(Wrapped e) -> return $ Left e
  where
    -- If createFileLink is successful this thread is responsible for deleting
    -- the link, so we do no subsequent actions in 'acquire' because they might
    -- fail. If createFileLink fails, it's either an expected error which we
    -- wrap and rethrow, or an unexpected error which we rethrow as-is.
    acquire pid =
        createFileLink (show pid) path
        `catch`
        \e -> if
            | isAlreadyExistsError e -> throwWrapped PidSymlinkPathAlreadyExists{exc=e}
            | otherwise -> throwIO e
    -- The release handler isn't run twice in normal operation (despite how
    -- bracket looks), so if the symlink is missing then somebody deleted it
    -- while we were running. That's an error, though not a useful one.
    release =
        removeFile path
        `catch`
        \e -> if
            | isDoesNotExistError e -> hPutStrLn stderr $ show PidSymlinkMissingOnCleanup{exc=e}
            | otherwise -> throwIO e
    -- Only run the user action if the symbolic link has our PID. It might
    -- not if another /process/ raced us in this same function.
    inner pid = do
        winner <- getSymbolicLinkTarget path
        if show pid == winner
        then return . Right =<< action
        else pure $ Left PidSymlinkRaceLost{winner}
