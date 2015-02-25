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
import Control.Applicative
import Data.IP
import Control.Monad as CM
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TQueue
import Data.Time.Clock
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.Serialize as DS
import Data.LargeWord
import Crypto.Random
import Data.Tuple as Tup
import Data.Maybe
import System.Timeout

import Network.BitTorrent.ClientControl

import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature, logger)
import Network.TCP.Proxy.Socks4 as Socks4

import Network.BitSmuggler.Common hiding (contactFiles)
import Network.BitSmuggler.Utils
import Network.BitSmuggler.Protocol
import Network.BitSmuggler.ARQ as ARQ

import Data.Map.Strict as Map
import Data.Set as Set

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
  , pieceProxy :: Async ()
}

-- https://www.youtube.com/watch?v=S0nlygb1Qfw

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

  -- make sure that when files are used up (they get completed)
  -- they are shrinked back to their original size
  -- TODO: reenable this
--  allocLinkedAsync $ async $ replenishFiles btClientConn (fst btProc) files

  -- wait for it...
  liftIO $ waitBoth (snd reverseProxy) (snd forwardProxy)
  return ()

-- server cleanup
cleanState stateVar = do
  state <- atomically $ readTVar stateVar
  forM (P.map snd $ Map.toList $ activeConns state) $ killConn
  return ()

killConn = cancel . handlerTask

serverConnInit secretKey stateVar handleConn fileFix direction local remote = do
  pieceHs <- makePieceHooks

  liftIO $ debugM logger $ "handling a connection to or from remote address "
                         P.++ (show remote)

  disconnectGate <- newGate 

  -- don't keep the proxy waiting and fork a worker
  -- to handle the connection
  forkIO $ handleConnection stateVar pieceHs secretKey handleConn disconnectGate


  streams <- fmap (if direction == Reverse then Tup.swap else P.id) $
                makeStreams pieceHs fileFix
  return $ DataHooks { incoming = P.fst streams
                     , outgoing = P.snd streams
                        -- TODO: implement 
                     , onDisconnect = do
                         debugM logger "!! DISCONNECT"
                         atomically $ openGate disconnectGate  -- signal the disconnect
                         return () 
                    } 


modActives f s = modifyTVar s (\s -> s {activeConns = f $ activeConns s})


-- TODO: this has become a huge-ass function - please break it down
handleConnection stateVar pieceHs secretKey userHandle disconnectGate = do
  -- classify connection 

  let noarq = noARQ -- there's no ARQ right now

  liftIO $ debugM logger $ "waiting for handshake message"

  -- a task to forward outgoing pieces without changing them
  -- until the initial message stage finishes 
  kill <- newEmptyTMVarIO
  forwardPieces <- async $ forwardUntilKilled (read (sendGetPiece pieceHs))
                                              (write (sendPutBack pieceHs)) 
                                              kill

  maybeFstMsg <- timeout (5 * 10 ^ 6) -- wait 5 secs
     $ runConduit $ (readSource (liftIO $ atomically $ read $ recvPiece pieceHs))
     =$ (recvPipe (recvARQ noarq) $ handshakeDecrypt secretKey) =$ DC.take 1

  case maybeFstMsg of
    Nothing -> do 
      -- timedout - just proxy the traffic 
      -- those threads just suck
      debugM logger "turns out it's not a bit-smuggler connection"

      drainIncoming <- async $ forever $ atomically $ read $ recvPiece pieceHs

      -- wait for the disconnect..
      atomically $ goThroughGate disconnectGate

      -- kill the threads meant to simply move the outgoing/incoming pieces
      cancel drainIncoming
      atomically $ putTMVar kill ()
    Just [fstClientMessage] -> do

       -- stop moving pieces after the connection has been recognized as bitsmuggler
      atomically $ putTMVar kill ()

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

          -- make some hooks specific to this particular connection
          connPieceHooks <- makePieceHooks
          let dataPipes = DataPipes controlPipe connPieceHooks dataGate
          
          pieceProxyTask <- async $ runPieceProxy pieceHs connPieceHooks

          task <- async $ runResourceT $ do
             -- schedule removal when this thread finishes
             register $ atomically $ modActives (Map.delete token) stateVar

             runConnection packetSize initGoBackNARQ
                           serverEncrypt (decrypt crypto) token
                           userHandle dataPipes

          atomically $ modActives (Map.insert token
                                  (Conn Nothing task dataPipes pieceProxyTask)) stateVar
          -- schedule disconnect cleanup
          forkIO $ disconnectCleanup disconnectGate token stateVar

          return ()     

        Control (ConnRequest keyRepr (Just token)) -> do
          -- client is requesting session recovery using token
          maybeConn <- atomically $ fmap (Map.lookup token . activeConns)
                                  $ readTVar stateVar
          case maybeConn of
            Just conn -> do
              -- TODO: implement session loading
              -- hint: use a proxy for incoming and out going bt pieces
              debugM logger $ "reloading client session with token " P.++ (show token)

              pieceProxyTask <- async $ runPieceProxy pieceHs (pieceHooks $ dataPipes conn)
              atomically $ modActives
                       (Map.adjust (\conn -> conn {pieceProxy = pieceProxyTask}) token)
                       stateVar

              -- schedule disconnect cleanup
              forkIO $ disconnectCleanup disconnectGate token stateVar
              return ()

            Nothing -> do
              errorM logger "session token not found"
              throwIO ClientProtocolError
        _ -> do
            errorM logger "The first client message should always be a conn request"
            throwIO ClientProtocolError
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
  userResult <- liftIO $ try' (userHandle $ pipeToConnData user)
  case userResult of
    Left e -> liftIO $ errorM logger "user handle terminated with exception "
    Right _ -> liftIO $ debugM logger "user handle terminated gracefully "

  -- termination handling
  liftIO $ flushMsgQueue (pipeSend user)

  return ()

-- FILE REPLENISHER
replenishWorkerCount = 2
replenishFiles btClientConn btProc files = runResourceT $ do
  jobQueue <- liftIO $ newTQueueIO
  setVar <- liftIO $ newTVarIO Set.empty
  CM.replicateM replenishWorkerCount $ allocLinkedAsync $ async
                                 $ replenishWorker btClientConn btProc files jobQueue setVar

  forever $ do
    liftIO $ threadDelay $ 5 * milli
    ts <- liftIO $ listTorrents btClientConn
    forM ts $ \t -> when (isUsedUp t) $ do
      let ih = torrentID t
      liftIO $ atomically $ do
        underWork <- fmap (Set.member ih) $ readTVar setVar
        when (not underWork) $ modifyTVar setVar (Set.insert ih) >> writeTQueue jobQueue ih

replenishWorker btClientConn btProc files jobQueue underWork = forever $ do
  infoHash <- atomically $ readTQueue jobQueue

  replenishFile btClientConn btProc files infoHash 
  -- once it's done it's no longer under work
  atomically $ modifyTVar underWork (Set.delete infoHash)


disconnectCleanup disconnectGate token stateVar = do
  -- wait at the gate till the disconnect is signalled
  atomically $ goThroughGate disconnectGate 
  now <- getCurrentTime
  maybeConn <- atomically $ do
    modActives (Map.adjust (\conn -> conn {onHoldSince = Just now}) token) stateVar
    state <- readTVar stateVar
    return $ Map.lookup token . activeConns $ state
  case maybeConn of
    (Just conn) -> cancel $ pieceProxy conn -- stop the piece proxy
    Nothing -> return () -- it's just not there anymore

handshakeDecrypt sk bs = fmap P.snd $ tryReadHandshake sk $ bs

-- this proxy runs on every connect until disconnect
-- it terminates when its thread is killed off
runPieceProxy src dest = runResourceT $ do

  -- clear up any leftover in the putback var
  liftIO $ atomically $ tryRead (sendPutBack dest)

  r <- runProxy recvPiece src dest
  sg <- runProxy sendGetPiece src dest
  sp <-runProxy sendPutBack dest src -- put back goes in the reverse direction
  forM (P.map snd [r, sg, sp]) $ liftIO . wait -- just block 
  return ()
  
runProxy op src dest =
  allocLinkedAsync $ async $ forever $  (atomically $ read (op src))
                                         >>=  (atomically . write (op dest))


forwardUntilKilled read write kill = do
  res <- atomically $ (fmap Left $ takeTMVar kill) `orElse` (fmap Right read)
  case res of 
    Left () -> return () -- terminate now
    Right v -> (atomically $ write v) >> forwardUntilKilled read write kill
