{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mog.MergeDriver.Handler where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (Async, async, poll)
import Control.Exception (SomeException, Handler(..), throwIO, catches, mask, finally, onException)
import Data.Maybe (catMaybes)
import Network.Socket (Socket, SockAddr)
import qualified Network.Socket as Socket hiding (Socket, SockAddr)

import Mog.MergeDriver.Args (Args(..))
import Mog.MergeDriver.Main (Request(..), Response(..), send, recvOrThrow, withSock, say)

-- TODO: This module needs a parameter to represent the domain-specific three
-- way merge.
--
-- * Use `mergedResultPathname` to find a function in the IR type.
-- * Deserialize and do domain-specific three way merge on:
--     `cwd<>mergeAncestor`
--     `cwd<>currentVersion`
--     `cwd<>otherBranchVersion`
-- * Write the result in `cwd<>currentVersion`.
-- * Return success/failure over the socket.

requestHandler :: Socket -> SockAddr -> IO ()
requestHandler csoc addr = do
    -- TODO: a background thread to cancel this after a timeout
    recvOrThrow csoc >>= \case
        AreYouStillThere -> send csoc StillAlive
        MergeRequest{cwd,args} -> do
            say $ "todo, merge this: " ++ show (cwd, args)

withUDLSock :: FilePath -> (Socket -> IO a) -> IO a
withUDLSock socketPath action =
    withSock $ \lsoc -> do
        say $ "bind " ++ socketPath
        Socket.bind lsoc $ Socket.SockAddrUnix socketPath
        say "listen"
        Socket.listen lsoc 1
        action lsoc

mergeDriverHandlerLoop :: Socket -> IO ()
mergeDriverHandlerLoop s = loop s []
  where
    -- loop mostly blocks inside acceptAsync
    loop lsoc threads =
        catches
            ((:) <$> acceptAsync lsoc requestHandler <*> reap threads)
            [ Handler $ \exc -> do
                ts <- reap threads
                if null ts
                then say "clean exit"
                else say $ show (length ts) ++ " threads may terminate uncleanly"
                throwIO (exc :: SomeException)
            ]
        >>= loop lsoc
    -- say when there are no threads or when one dies with an exception
    reap :: [Async ()] -> IO [Async ()]
    reap [] = say "it is quiet" >> return []
    reap ts = catMaybes <$> mapM reapOne ts
    reapOne t = poll t >>= \case
        Nothing         -> pure $ Just t -- thread is still running
        Just (Right ()) -> pure Nothing -- thread closed gracefully
        Just (Left exc) -> say ("a thread died: " ++ show exc) >> return Nothing

acceptAsync :: Socket -> (Socket -> SockAddr -> IO ()) -> IO (Async ())
acceptAsync lsoc action =
    mask $ \restore -> do
        (csoc, addr) <- restore $ Socket.accept lsoc
        async (restore (action csoc addr) `finally` Socket.close csoc)
            `onException` Socket.close csoc -- in case async fails

acceptFork :: Socket -> (Socket -> SockAddr -> IO ()) -> IO ThreadId
acceptFork lsoc action =
    mask $ \restore -> do
        (csoc, addr) <- restore $ Socket.accept lsoc
        forkIO (restore (action csoc addr) `finally` Socket.close csoc)
            `onException` Socket.close csoc -- in case forkIO fails
