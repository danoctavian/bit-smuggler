{-# LANGUAGE DeriveDataTypeable #-}

module Network.BitSmuggler.StreamMultiplex where

import Data.Typeable
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.Conduit.Cereal
import qualified Network.TCP.Proxy.Server as Proxy
import Control.Exception
import Data.Map as Map
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async
import Data.Serialize as DS
import Data.Word
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Network.BitSmuggler.Utils
import Network.BitSmuggler.Protocol

{-
multiplexer for streams of bytestring

-}


data Multiplexer i o = Multiplexer {
                    activeConns ::  TVar (Map ConnId (Conn i))
                  , outgoingQ :: TQueue (MuxMessage o) 
                  , lastConnId :: TVar ConnId
                }


type ConnId = Word32 -- a unique identifier for the connection
data Conn a = Conn {msgQ :: TQueue (WireMessage a)}

-- messages 

-- all messages are labelled with an ID
data MuxMessage a = MuxMessage ConnId (WireMessage a)

data ServerControl = ConnSuccess | ConnFail 
  deriving (Show, Eq)
data ClientControl = ConnnectTo Proxy.RemoteAddr
  deriving (Show, Eq)

data MultiplexException = ConnectionFailed 
  deriving (Show, Typeable)
instance Exception MultiplexException 

type InitConn = Proxy.RemoteAddr -> (ConnData -> IO ()) -> IO ()

dispatch activeConns onRecordMiss = awaitForever  $ \msg@(MuxMessage connId m) -> do
  actives <- liftIO $ atomically $ readTVar activeConns
  case Map.lookup connId actives of
    (Just conn) -> liftIO $ atomically $ writeTQueue (msgQ conn) m
    Nothing -> onRecordMiss msg

withClient :: ConnData -> (InitConn -> IO ()) -> IO ()
withClient sharedPipe handler = do
  mux <- initMux
  let init = initConn mux
 
  _ <- runConcurrently $ (,,)
    <$> Concurrently (sourceTQueue (outgoingQ mux) =$ conduitPut putLenPrefixed
                      $$ (connSink sharedPipe))
    <*> Concurrently (connSource sharedPipe
                      =$ conduitGet (getLenPrefixed :: Get (MuxMessage ServerControl))
                      $$ dispatch (activeConns mux) (\ _ -> return ()))
    <*> Concurrently (handler init)

  return ()

initMux = Multiplexer <$> newTVarIO Map.empty <*> newTQueueIO <*> newTVarIO 0

isData (Data _) = True
fromData (Data d) = d

initConn mux remote handle = do 
  incomingQ <- newTQueueIO 

  connId <- atomically $ do
    lastId <- readTVar $ lastConnId mux
    writeTVar (lastConnId mux) (succ lastId) -- just increment
    return lastId
 
  -- send request to connect 
  atomically $ writeTQueue (outgoingQ mux)
             $ MuxMessage connId $ Control $ ConnnectTo remote 
  (Control connResult) <- atomically $ readTQueue incomingQ
  case connResult of 
    ConnFail -> throwIO ConnectionFailed
    ConnSuccess -> do
        (handle $ ConnData  {
              connSource = toProducer
                         $ sourceTQueue incomingQ =$ DC.filter isData
                         =$ DC.map fromData =$ dataStreamConduit 
            , connSink = DC.map (MuxMessage connId . Data . DataChunk)
                         =$ sinkTQueue (outgoingQ mux)
           }) 
          `finally` (return ())

-- serialization
instance (Serialize a) => Serialize (MuxMessage a) where
  put (MuxMessage id m) = putWord32be id >> put m
  get =  MuxMessage <$> getWord32be <*> get

instance Serialize ServerControl where
  put ConnSuccess = putWord8 0
  put ConnFail = putWord8 1
  get = (byte 0 >> return ConnSuccess) <|> (byte 1 >> return ConnFail)

instance Serialize ClientControl where
  put (ConnnectTo remote) = undefined 
  get = undefined

