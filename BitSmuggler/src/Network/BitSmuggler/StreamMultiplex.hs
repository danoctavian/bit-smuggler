{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.BitSmuggler.StreamMultiplex where

import Data.Typeable
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.Conduit.Cereal
import Data.Conduit.Network
import qualified Network.TCP.Proxy.Server as Proxy
import Control.Exception
import Data.Map as Map
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import Data.Serialize as DS
import Data.Word
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC


import Network.BitSmuggler.Utils
import Network.BitSmuggler.Protocol

{-
multiplexer for streams of bytestring

this is generalized as it should be.

-}

data Multiplexer i o = Multiplexer {
                    activeConns ::  TVar (Map ConnId (Conn i))
                  , outgoingQ :: TQueue (MuxMessage o) 
                }


type ConnId = Word32 -- a unique identifier for the connection
data Conn a = Conn {msgQ :: TQueue (WireMessage a)}

-- messages 

-- all messages are labelled with an ID
data MuxMessage a = MuxMessage ConnId (WireMessage a)

data ServerControl = ConnSuccess | ConnFail 
  deriving (Show, Eq)
data ClientControl = ConnectTo Proxy.RemoteAddr
  deriving (Show, Eq)

data MultiplexException = ConnectionFailed 
  deriving (Show, Typeable)
instance Exception MultiplexException 

type InitConn = Proxy.RemoteAddr -> (ConnData -> IO ()) -> IO ()

dispatch activeConns onRecordMiss = awaitForever  $ \msg@(MuxMessage connId m) -> do
  actives <- liftIO $ atomically $ readTVar activeConns
  case Map.lookup connId actives of
    (Just conn) -> liftIO $ atomically $ writeTQueue (msgQ conn) m
    Nothing -> liftIO $ onRecordMiss msg


addrToBS remote = BSC.pack $ case remote of
                               Left hostName -> hostName
                               Right ip -> show ip

handleConn mux (MuxMessage connId (Control (ConnectTo remote))) = void $ forkIO $ do
  handle (\(e :: SomeException) -> -- on exception we fail
    atomically $ writeTQueue (outgoingQ mux)
               $ MuxMessage connId $ Control $ ConnFail) $

    runTCPClient (clientSettings (fromIntegral . snd $ remote) (addrToBS . fst $ remote))
    $ \appData -> do
      -- connection is succesful therefore we notify
      atomically $ writeTQueue (outgoingQ mux) $ MuxMessage connId $ Control $ ConnSuccess

      -- and create a record of it
      incomingQ <- newTQueueIO 
      atomically $ modifyTVar (activeConns mux) (Map.insert connId (Conn incomingQ))

      -- proxy data back and forth
      void $ concurrently
        (appSource appData =$ DC.map (MuxMessage connId . Data . DataChunk) 
                           $$ sinkTQueue (outgoingQ mux))
        (incomingPipe incomingQ $$ (appSink appData))


runServer :: ConnData -> (ConnData -> IO ()) -> IO ()
runServer sharedPipe handle = do
  mux <- initMux
  _ <- runConcurrently $ (,)
    <$> Concurrently (sourceTQueue (outgoingQ mux) =$ conduitPut putLenPrefixed
                      $$ (connSink sharedPipe))
    <*> Concurrently (connSource sharedPipe
                      =$ conduitGet (getLenPrefixed :: Get (MuxMessage ClientControl))
                      $$ dispatch (activeConns mux) (handleConn mux))
  return ()
  

runClient :: ConnData -> (InitConn -> IO ()) -> IO ()
runClient sharedPipe handle = do
  mux <- initMux
  connIdVar <- newTVarIO 0
  let init = initConn mux connIdVar
 
  _ <- runConcurrently $ (,,)
    <$> Concurrently (sourceTQueue (outgoingQ mux) =$ conduitPut putLenPrefixed
                      $$ (connSink sharedPipe))
    <*> Concurrently (connSource sharedPipe
                      =$ conduitGet (getLenPrefixed :: Get (MuxMessage ServerControl))
                      $$ dispatch (activeConns mux) (\ _ -> return ()))
    <*> Concurrently (handle init)

  return ()

initMux = Multiplexer <$> newTVarIO Map.empty <*> newTQueueIO

isData (Data _) = True
fromData (Data d) = d

initConn mux lastConnId remote handle = do 
  incomingQ <- newTQueueIO 

  connId <- atomically $ do
    lastId <- readTVar $ lastConnId
    writeTVar lastConnId (succ lastId) -- just increment
    return lastId

  atomically $ modifyTVar (activeConns mux) (Map.insert connId (Conn incomingQ))
 
  -- send request to connect 
  atomically $ writeTQueue (outgoingQ mux)
             $ MuxMessage connId $ Control $ ConnectTo remote 
  (Control connResult) <- atomically $ readTQueue incomingQ
  case connResult of 
    ConnFail -> throwIO ConnectionFailed
    ConnSuccess -> do
        (handle $ ConnData  {
              connSource = toProducer $ incomingPipe incomingQ
           , connSink = DC.map (MuxMessage connId . Data . DataChunk)
                         =$ sinkTQueue (outgoingQ mux)
           }) 
          `finally`  --cleanup
             (do
                atomically $ writeTQueue (outgoingQ mux)
                           $ MuxMessage connId $ Data $ EndOfStream 
                atomically $ modifyTVar (activeConns mux) (Map.delete connId))


incomingPipe incomingQ = sourceTQueue incomingQ =$ DC.filter isData
                         =$ DC.map fromData =$ dataStreamConduit 
 

-- serialization
instance (Serialize a) => Serialize (MuxMessage a) where
  put (MuxMessage id m) = putWord32be id >> put m
  get =  MuxMessage <$> getWord32be <*> get

instance Serialize ServerControl where
  put ConnSuccess = putWord8 0
  put ConnFail = putWord8 1
  get = (byte 0 >> return ConnSuccess) <|> (byte 1 >> return ConnFail)

instance Serialize ClientControl where
  put (ConnectTo remote) = undefined 
  get = undefined
