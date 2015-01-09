module Network.BitSmuggler.Server where

import Prelude as P
import Network.BitSmuggler.Crypto (Key)
import System.Log.Logger
import Data.Word
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString
import Control.Monad.IO.Class
import Data.IP
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Time.Clock

import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature)
import Network.TCP.Proxy.Socks4 as Socks4

import Network.BitSmuggler.Common hiding (contactFiles)
import Network.BitSmuggler.Utils

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
    activeConnections :: Map (IP, PortNum) Connection
}

data Connection = Conn {
    onHoldSince :: UTCTime
}

listen :: ServerConfig -> (ConnData -> IO ()) -> IO ()
listen config handle = runResourceT $ do
  liftIO $ debugM logger "started bit-smuggler server..."

  let btConf = btClientConfig config
  -- start torrent client (with config)
  (btProc, btClientConn) <- setupBTClient $ btClientConfig config

  -- setup the files on which the client is working in a temp dir
  files <- setupContactFiles (contactFiles config) (fileCachePath config)
 
  serverState <- liftIO $ newTVarIO $ ServerState {activeConnections = Map.empty}
  let onConn = serverConnInit serverState

  -- setup proxies
  -- reverse
  allocAsync $ async $ Proxy.run $ Proxy.Config {
                 proxyPort = revProxyPort btConf
               , initHook = P.flip onConn 
               , handshake = revProxy (read localhost :: IP) (pubBitTorrentPort btConf)
             }

  -- forward socks 
  allocAsync $ async $ Proxy.run $ Proxy.Config {
                 proxyPort = socksProxyPort btConf
               , initHook = onConn 
               , handshake = Socks4.serverProtocol
             }
  -- tell client to use the files

  -- wait for it...
  -- in case of torrent app crash - restart it 
  return ()


serverConnInit stateVar local remote = do
  -- check if connection to this remote address is still active
  --  atomically $ do readTVar


  return undefined

{-
  liftIO $ debugM Server.logger "initializing reverse proxy connection..."
  [incomingPipe, outgoingOut] <- replicateM 2 (liftIO $ atomically newTChan)
  outgoingIn <- (liftIO $ atomically newTChan)
  let outgoingPipe = (outgoingIn, outgoingOut) :: (TChan Packet, TChan ByteString)
  -- TODO; receive packets until you get the bootstrap package or timeout is reached
  liftIO $ forkIO $ checkForClientConn incomingPipe outgoingPipe bootstrapEnc $
                    makeClientConn incomingPipe outgoingPipe handleConnection
  -- while just forwarding the sent packages...
  liftIO $ forkIO $ noOpStreamOutgoing outgoingPipe

  loader <- liftIO $ pieceLoader torrentFilePath filePath
  return $ PacketHandlers {
   -- incoming packets are packets from the Bittorrent reverse proxied server
    incoming = (pieceHandler $ \i off -> transformPacket outgoingPipe),
    outgoing = (pieceHandler $ (pieceFix (toGetPieceBlock loader) $ collectPacket incomingPipe))
    -- outgoing packets are packets send by the connection initiator, which is the client
  }


checkForClientConn incomingPipe (inputOutgoing, outputOutgoing) bootstrapEnc makeConnection = do
  liftIO $ debugM Server.logger "reading greeting message..."
  greeting <- liftIO $ timeout (10 ^ 8) $ (incomingPackageSource incomingPipe
                                      (bootstrapServerDecrypt bootstrapEnc)) $$ (DCL.take 1)
  liftIO $ debugM Server.logger "got something for a greeting"
  case greeting of
    Just [ClientGreeting bs] -> do
      liftIO $ debugM Server.logger "got a good result lads!" 
      liftIO $ atomically $ writeTChan inputOutgoing Kill -- kill the noOp thread
      makeConnection $ makeServerEncryption bootstrapEnc bs
    other ->  liftIO $ do
        debugM Server.logger $ "it's not a client connection. run a normal proxy. Received" ++ (show other)
        -- TODO: clean this up... it's a waste of computation
        forever (liftIO $ atomically $ readTChan incomingPipe) -- just emtpying the chan...
  return ()
-}

revProxy ip port = return $ ProxyAction {
                            command = CONNECT
                          , remoteAddr = (Right ip, port)
                          , onConnection = \ _ -> return () 
                          }


