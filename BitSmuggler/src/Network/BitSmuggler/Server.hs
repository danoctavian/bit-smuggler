{-# LANGUAGE RecordWildCards, TupleSections #-}
module Network.BitSmuggler.Server where

import Prelude as P hiding (read)
import Network.BitSmuggler.Crypto as Crypto
import System.Log.Logger
import Data.Word
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Control.Monad.IO.Class
import Data.IP
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Time.Clock
import Data.Conduit as DC
import Data.Conduit.List as DC
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

data ServerConfig = ServerConfig {
    serverSecretKey :: Key
  , btClientConfig :: BTClientConfig
  -- the files on which the server is "listening"
  , contactFiles :: [ContactFile]
  , fileCachePath :: FilePath
}

data ServerState = ServerState {
    activeConns :: Map SessionToken Connection
}

data Connection = Conn {
    onHoldSince :: Maybe UTCTime
  , handlerTask :: Async ()
  , dataPipes :: DataPipes ClientMessage ServerMessage
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

  liftIO $ debugM logger "finished initialization"

  -- tell bittorrent client to use the files
  -- TODO: implement
  liftIO $ addTorrents btClientConn (fst btProc) files

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
  pieceHs <- makePieceHooks

  liftIO $ debugM logger $ "handling a connection to or from remote address "
                         P.++ (show remote)

  -- don't keep the proxy waiting and fork a worker
  -- to handle the connection
  forkIO $ handleConnection stateVar pieceHs secretKey handleConn

  streams <- fmap (if direction == Reverse then Tup.swap else P.id) $
                makeStreams pieceHs fileFix
  return $ DataHooks { incoming = P.fst streams
                     , outgoing = P.snd streams
                        -- TODO: implement 
                     , onDisconnect = debugM logger "!! DISCONNECT" >> return () 
                    } 


modActives f s = atomically $ modifyTVar s
  (\s -> s {activeConns = f $ activeConns s})


handleConnection stateVar pieceHooks secretKey userHandle = do
  -- classify connection 

  let noarq = noARQ -- there's no ARQ right now
  let packetSize = blockSize - Crypto.msgHeaderLen

  liftIO $ debugM logger $ "waiting for handshake message"

  [fstClientMessage] <-
    runConduit $ (readSource (liftIO $ read $ recvPiece pieceHooks))
               =$ (recvPipe (recvARQ noarq) $ handshakeDecrypt secretKey) =$ DC.take 1

  liftIO $ debugM logger $ "received first message  from client !"

  case fstClientMessage of
    (Control (ConnRequest keyRepr Nothing)) -> do
      -- client is requesting the creation of a new session

      liftIO $ debugM logger $ "client requests new session "

      let crypto = makeServerEncryption secretKey keyRepr
      initCprg <- liftIO $ makeCPRG
      let (token, cprg) = cprgGenerate tokenLen initCprg
      let serverEncrypt = encrypter (encrypt crypto) cprg
      
      -- control messages   
      controlSend <-liftIO $ (newTQueueIO :: IO (TQueue ServerMessage))
      controlRecv <- liftIO $ (newTQueueIO :: IO (TQueue ClientMessage))
      let controlPipe = Pipe controlRecv controlSend
  -- to handle the connection
      dataGate <- liftIO $ newGate 
      let dataPipes = DataPipes controlPipe pieceHooks dataGate

      task <- async $ runResourceT $ do
         -- schedule removal when this thread finishes
         register $ modActives (Map.delete token) stateVar

         runConnection packetSize noarq
                       serverEncrypt (decrypt crypto) token
                       userHandle dataPipes

      modActives (Map.insert token (Conn Nothing task dataPipes))
                 stateVar
      return ()     

    Control (ConnRequest keyRepr (Just token)) -> do
      -- client is requesting session recovery using token
      maybeConn <- atomically $ fmap (Map.lookup token . activeConns) $ readTVar stateVar
      case maybeConn of
        Just conn -> do
          -- TODO: implement session loading
          -- hint: use a proxy for incoming and out going bt pieces
          errorM logger "session loading not implemented!"
          throwIO UnsupportedFeature 
        Nothing -> do
          errorM logger "session token not found"
          throwIO ClientProtocolError
    _ -> do
        errorM logger "The first client message should always be a conn request"
        throwIO ClientProtocolError
  -- store conn  
  return ()

runConnection packetSize arq encrypter decrypt token userHandle ps@(DataPipes {..}) = do
  user <- launchPipes packetSize arq encrypter decrypt ps
  -- reply to handshake 
  -- accept. we don't discriminate.. for now

  liftIO $ debugM logger $ "sending accept message to client"

  liftIO $ atomically $ writeTQueue (pipeSend controlPipe) $ AcceptConn token

  -- allowing data to flow
  liftIO $ atomically $ openGate dataGate

  liftIO $ debugM logger $ "running the user conn handler"
  -- run conn handler
  liftIO $ userHandle $ pipeToConnData user
  return ()

 
handshakeDecrypt sk bs = fmap P.snd $ tryReadHandshake sk $ bs

