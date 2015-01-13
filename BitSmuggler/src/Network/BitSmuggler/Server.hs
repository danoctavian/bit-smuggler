{-# LANGUAGE RecordWildCards, TupleSections #-}
module Network.BitSmuggler.Server where

import Prelude as P
import Network.BitSmuggler.Crypto as Crypto
import System.Log.Logger
import Data.Word
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString
import Control.Monad.IO.Class
import Data.IP
import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Time.Clock
import Data.Conduit as DC
import Data.Serialize as DS
import Data.LargeWord
import Crypto.Random
import Data.Tuple as Tup

import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature)
import Network.TCP.Proxy.Socks4 as Socks4

import Network.BitSmuggler.Common hiding (contactFiles)
import Network.BitSmuggler.Utils
import Network.BitSmuggler.Protocol
import Network.BitSmuggler.ARQ as ARQ

import Data.Map.Strict as Map

{-

SERVER.

run single torrent client - running many potentially blows the cover
 a normal peer in the bittorrent network runs a single instance of
 some torrent client

-}

logger = "BitSmuggler.Server"

data ServerConfig = ServerConfig {
    serverSecretKey :: Key
  , btClientConfig :: BTClientConfig
  -- the files on which the server is "listening"
  , contactFiles :: [ContactFile]
  , fileCachePath :: FilePath
}

data ServerState = ServerState {
    activeConns :: Map (IP, PortNum) Connection
}

data Connection = Conn {
    onHoldSince :: Maybe UTCTime
  , pieceHooks :: PieceHooks
  , handlerTask :: Async ()
}


listen :: ServerConfig -> (ConnData -> IO ()) -> IO ()
listen config handle = runResourceT $ do
  liftIO $ debugM logger "started bit-smuggler server..."

  let btConf = btClientConfig config
  -- start torrent client (with config)
  (btProc, btClientConn) <- setupBTClient $ btClientConfig config

  -- setup the files on which the client is working in a temp dir
  files <- setupContactFiles (contactFiles config) (fileCachePath config)
 
  serverState <- liftIO $ newTVarIO $ ServerState {activeConns = Map.empty}
  register $ cleanState serverState

  let fileFixer = findPieceLoader files
  let onConn = serverConnInit (serverSecretKey config) serverState handle fileFixer

  -- setup proxies

  (reverseProxy, forwardProxy) <- startProxies (btClientConfig config) onConn
  -- tell client to use the files

  -- wait for it...
  liftIO $ waitBoth (snd reverseProxy) (snd forwardProxy)
  return ()

-- server cleanup
cleanState stateVar = do
  state <- atomically $ readTVar stateVar
  forM (P.map snd $ Map.toList $ activeConns state) $ killConn
  return ()

killConn = cancel . handlerTask

-- TODO: check this out https://www.youtube.com/watch?v=uMK0prafzw0
serverConnInit secretKey stateVar handleConn fileFix direction local remote = do
  -- check if connection to this remote address is still active
  maybeConn <- atomically $ fmap (Map.lookup remote . activeConns) $ readTVar stateVar

  conn <- case maybeConn of 
    Just conn -> return conn
    Nothing -> do
      -- rip it a new one
      [recv, sendGet, sendPut] <- replicateM 3 (liftIO $ newTQueueIO)
      let pieceHs = PieceHooks recv sendGet sendPut
      handleTask <- async $ runResourceT $ do
         register $ modActives (Map.delete remote)  stateVar
         handleConnection pieceHs secretKey handleConn

      let conn = Conn Nothing pieceHs handleTask
      modActives (Map.insert remote conn) stateVar

      return conn        

  let streams = (if direction == Reverse then Tup.swap else P.id) $
                makeStreams (pieceHooks conn) fileFix
  return $ DataHooks {incoming = P.fst streams, outgoing = P.snd streams} 


modActives f s = atomically $ modifyTVar s
  (\s -> s {activeConns = f $ activeConns s})


handleConnection (PieceHooks {..}) secretKey userHandle = do
  let (recvARQ, sendARQ) = noARQ -- there's no ARQ right now
  let packetSize = blockSize - Crypto.msgHeaderLen
  
  userSend <- liftIO $ (newTQueueIO :: IO (TQueue ServerMessage))
  userRecv <- liftIO $ (newTQueueIO :: IO (TQueue ClientMessage))

  -- launch receive pipe
  allocLinkedAsync $ async
          $ (queueSource recvPiece) =$ (recvPipe recvARQ $ serverDecrypter secretKey)
          $$ queueSink userRecv

  -- wait for handshake
  (ConnRequest payload) <- liftIO $ atomically $ readTQueue userRecv

  let crypto = makeServerEncryption secretKey payload
  cprg <- liftIO $ makeCPRG

  -- launch send pipe
  allocLinkedAsync $ async
         $ (tryQueueSource userSend) =$ sendPipe packetSize sendARQ (encrypter crypto cprg) 
                $$ outgoingSink (liftIO $ atomically $ readTQueue sendGetPiece)
                            (\p -> liftIO $ atomically $ writeTQueue sendPutBack p)

  -- reply to handshake 
  -- accept. we don't discriminate.. for now
  liftIO $ atomically $ writeTQueue userSend AcceptConn  

  -- run conn handler
  liftIO $ userHandle $ ConnData {
                          connSend = atomically . writeTQueue userSend . ServerData
                        , connRecv = fmap unwrapData $ atomically $ readTQueue userRecv}
  return ()

unwrapData (ClientData d) = d

serverDecrypter sk = Decrypter {
  runD = \bs -> do
         (cryptoOps, msg) <- tryReadHandshake sk bs
         let applyDecrypt = Decrypter {runD = \bs ->
                                        fmap (, applyDecrypt) $ decrypt cryptoOps bs}    
         return (msg, applyDecrypt)
} 

encrypter ops cprg = Encrypter {
  runE = \bs -> let (bytes, next) = cprgGenerate 16 cprg
          in (encrypt ops (fromRight $ DS.decode bytes :: Word128) bs, encrypter ops next)
}

revProxy ip port = return $ ProxyAction {
                            command = CONNECT
                          , remoteAddr = (Right ip, port)
                          , onConnection = \ _ -> return () 
                          }

