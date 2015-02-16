{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.BitSmuggler.StreamMultiplex where

import Prelude as P
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
                  , outgoingQ :: TQueue MuxMessage
                }


type ConnId = Word32 -- a unique identifier for the connection
data Conn a = Conn {msgQ :: TQueue DataUnit}

-- messages 

data MuxMessage = MuxMessage ConnId DataUnit

data ServerControl = ConnSuccess | ConnFail 
  deriving (Show, Eq)
data ClientControl = ConnectTo Proxy.RemoteAddr
  deriving (Show, Eq)

data MultiplexException = ConnectionFailed 
  deriving (Show, Typeable)
instance Exception MultiplexException 

type InitConn = (ConnData -> IO ()) -> IO ()


runServer :: ConnData -> (ConnData -> IO ()) -> IO ()
runServer sharedPipe onConnection = do
  mux <- initMux
  void $ runConcurrently $ concurrentProxy sharedPipe mux (handleConn mux onConnection)

handleConn mux onConnection (MuxMessage connId smth) = void $ forkIO $ do
  incomingQ <- newTQueueIO 

  atomically $ writeTQueue incomingQ smth -- push the first message

  atomically $ modifyTVar (activeConns mux) (Map.insert connId (Conn incomingQ))

  -- run user supplied handle and catch all exceptions
  connOutcome <- try' $ onConnection $ ConnData {
       connSource = toProducer $ incomingPipe incomingQ
     , connSink = DC.map (MuxMessage connId . DataChunk)
                   =$ sinkTQueue (outgoingQ mux)
     }
  cleanupConn mux connId

runClient :: ConnData -> (InitConn -> IO ()) -> IO ()
runClient sharedPipe handle = do
  mux <- initMux
  connIdVar <- newTVarIO 0
  let init = initConn mux connIdVar
 
  void $ runConcurrently $ concurrentProxy sharedPipe mux (\_ -> return ())
                         *> Concurrently (handle init)


-- forward traffic to and from
concurrentProxy sharedPipe mux onRecordMiss = 
        Concurrently (sourceTQueue (outgoingQ mux) =$ conduitPut putLenPrefixed
                      $$ (connSink sharedPipe))
     *> Concurrently (connSource sharedPipe
                      =$ conduitGet (getLenPrefixed :: Get MuxMessage)
                      $$ dispatch (activeConns mux) onRecordMiss)


initMux = Multiplexer <$> newTVarIO Map.empty <*> newTQueueIO

isData (Data _) = True
fromData (Data d) = d

initConn mux lastConnId handle = do 
  connId <- atomically $ do
    lastId <- readTVar $ lastConnId
    writeTVar lastConnId (succ lastId) -- just increment
    return lastId

  incomingQ <- newTQueueIO 
  atomically $ modifyTVar (activeConns mux) (Map.insert connId (Conn incomingQ))

  (handle $ ConnData  {
        connSource = toProducer $ incomingPipe incomingQ
     , connSink = DC.map (MuxMessage connId . DataChunk)
                   =$ sinkTQueue (outgoingQ mux)
     }) 
     -- on termination, we cleanup and let the exception bubble up (client-side)
    `finally` (cleanupConn mux connId) 


dispatch activeConns onRecordMiss = awaitForever  $ \msg@(MuxMessage connId m) -> do
  actives <- liftIO $ atomically $ readTVar activeConns
  case Map.lookup connId actives of
    (Just conn) -> liftIO $ atomically $ writeTQueue (msgQ conn) m
    Nothing -> liftIO $ onRecordMiss msg


incomingPipe incomingQ = sourceTQueue incomingQ =$ dataStreamConduit 

cleanupConn mux connId = do
  atomically $ writeTQueue (outgoingQ mux) $ MuxMessage connId $ EndOfStream 
  atomically $ modifyTVar (activeConns mux) (Map.delete connId)


-- serialization
instance Serialize MuxMessage where
  put (MuxMessage id m) = putWord32be id >> put m
  get =  MuxMessage <$> getWord32be <*> get

instance Serialize ServerControl where
  put ConnSuccess = putWord8 0
  put ConnFail = putWord8 1
  get = (byte 0 >> return ConnSuccess) <|> (byte 1 >> return ConnFail)

instance Serialize ClientControl where
  put (ConnectTo remote) = undefined 
  get = undefined
