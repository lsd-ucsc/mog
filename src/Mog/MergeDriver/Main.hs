{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Module for the main-function stub via which git will call back into our
-- program. This main-function is only a stub because it will pass the data
-- from git over a socket, wait for a response, and then terminate. The work of
-- merging is performed in the main process.
module Mog.MergeDriver.Main where

import Control.Concurrent (myThreadId)
import Control.Exception (bracket, assert, throwIO)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Socket (Socket)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getEnvironment)
import System.Exit (die)
import System.IO (stderr, hPutStrLn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text hiding (Text)
import qualified Network.Socket as Socket hiding (Socket)
import qualified Network.Socket.ByteString as SocketBS

import Codec.Serialise (Serialise, serialise, deserialiseOrFail)
import Options.Generic (getRecord, getRecordPure)

import Mog.MergeDriver.Args (Args(..))

data Request
    = MergeRequest {cwd::FilePath, args::Args}
    | AreYouStillThere
    deriving (Generic, Show)

data Response
    = MergeResult {ok::Bool}
    | StillAlive
    deriving (Generic, Show)

instance Serialise Request
instance Serialise Response

-- | Wrap your main with this, before initializing other resources such as GUI
-- or sockets.
--
-- * If @DEBUG_MOG_MERGE_DRIVER@ envvar is set: Run the merge driver.
-- * If commandline arguments are perfect for the merge driver: Run the merge driver.
-- * Otherwise: Run your main.
--
-- TODO: remove @DEBUG_MOG_MERGE_DRIVER@ path
withMergeDriver :: IO () -> IO ()
withMergeDriver userMain = do
    debug <- lookup "DEBUG_MOG_MERGE_DRIVER" <$> getEnvironment
    case debug of
        Just{} -> mergeDriverMain =<< getRecord programDescription
        Nothing -> maybe userMain mergeDriverMain . getRecordPure . map Text.pack =<< getArgs

mergeDriverMain :: Args -> IO ()
mergeDriverMain args = Socket.withSocketsDo $ do
    let say x = hPutStrLn stderr $ "[merge driver] " ++ x
    withSock $ \sock -> do
        say "connect"
        Socket.connect sock (Socket.SockAddrUnix $ socketPath args)

        say "sending message"
        cwd <- getCurrentDirectory
        send sock MergeRequest{cwd, args=args{socketPath=""}}

        say "receive message"
        -- TODO: a background thread to cancel this after a timeout
        recvOrThrow sock >>= \case
            MergeResult{ok=True} -> say "done"
            MergeResult{ok=False} -> die "merge failed"
            StillAlive -> die "unexpected response"

programDescription :: Text
programDescription = "MoG merge driver - Pass arguments to a running instance of a program using MoG over a unix domain socket."

send :: Serialise a => Socket -> a -> IO ()
send sock msg = do
    let raw = BSL.toStrict $ serialise msg
    sent <- SocketBS.send sock raw
    assert (sent == BS.length raw) (return ())

recvOrThrow :: Serialise a => Socket -> IO a
recvOrThrow sock = do
    raw <- SocketBS.recv sock 4096
    let em = deserialiseOrFail $ BSL.fromStrict raw
    either throwIO return em

-- | TODO: define this somewhere else
withSock :: (Socket -> IO ()) -> IO ()
withSock =
    bracket
        (Socket.socket Socket.AF_UNIX Socket.Datagram Socket.defaultProtocol)
        Socket.close

-- | TODO: define this somewhere else
say :: String -> IO ()
say msg = do
    me <- myThreadId
    hPutStrLn stderr $ show me ++ ": " ++ msg
