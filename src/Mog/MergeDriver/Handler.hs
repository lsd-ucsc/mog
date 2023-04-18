{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mog.MergeDriver.Handler where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.Async (Async, async, poll)
import Control.Exception (SomeException, Handler(..), throwIO, catches, mask, finally, onException)
import Data.Maybe (catMaybes)
import Network.Socket (Socket, SockAddr)
import Data.Proxy (Proxy(..))
import System.FilePath ((</>))
import qualified Network.Socket as Socket hiding (Socket, SockAddr)
import qualified Data.ByteString.Lazy as BSL

import Mog.MergeDriver.Args (Args(..))
import Mog.MergeDriver.Main (Request(..), Response(..), send, recvOrThrow, withSock, say)
import Mog.MergeDriver.Merge (FindAndMerge(..), schemaPath)

requestHandler :: FindAndMerge s => Proxy s -> Socket -> SockAddr -> IO ()
requestHandler schema csoc _addr = do
    -- TODO: a background thread to cancel this after a timeout
    recvOrThrow csoc >>= \case
        AreYouStillThere -> send csoc StillAlive
        MergeRequest{cwd,args} ->
            (do mergeRequest cwd args
                send csoc MergeResult{ok=True})
            `finally`
                send csoc MergeResult{ok=False}
  where
    mergeRequest cwd args = do
        base <- BSL.readFile (cwd </> mergeAncestor args)
        ours <- BSL.readFile (cwd </> currentVersion args)
        theirs <- BSL.readFile (cwd </> otherBranchVersion args)
        path <- either throwIO return $ schemaPath (mergedResultPathname args)
        either throwIO (BSL.writeFile $ cwd </> currentVersion args)
            $ findMerge schema path base ours theirs

withUnixDomainListeningSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixDomainListeningSocket socketPath action =
    withSock $ \lsoc -> do
        say $ "bind " ++ socketPath
        Socket.bind lsoc $ Socket.SockAddrUnix socketPath
        say "listen"
        Socket.listen lsoc 1
        action lsoc

mergeDriverHandlerLoop :: FindAndMerge s => Socket -> Proxy s -> IO ()
mergeDriverHandlerLoop s schema = loop s []
  where
    -- loop mostly blocks inside acceptAsync
    loop lsoc threads =
        catches
            ((:) <$> acceptAsync lsoc (requestHandler schema) <*> reap threads)
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
