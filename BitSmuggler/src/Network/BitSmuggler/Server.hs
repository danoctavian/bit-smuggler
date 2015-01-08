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

import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature)
import Network.TCP.Proxy.Socks4 as Socks4

import Network.BitSmuggler.Common hiding (contactFiles)
import Network.BitSmuggler.Utils


{-

SERVER.

run single torrent client - running many potentially blows the cover
 a normal peer in the bittorrent network runs a single instance of
 some torrent client

-}

logger = "BitSmuggler.Server"


{-

rev proxy
socks proxy
bt client private port
bt client cmd server port


-}


data ServerConfig = ServerConfig {
    serverSecretKey :: Key
  , btClientConfig :: BTClientConfig
  -- the files on which the server is "listening"
  , contactFiles :: [ContactFile]
  , fileCachePath :: FilePath
}

listen :: ServerConfig -> (ConnData -> IO ()) -> IO ()
listen config handle = runResourceT $ do
  liftIO $ debugM logger "started bit-smuggler server..."

  let btConf = btClientConfig config
  -- start torrent client (with config)
  (btProc, btClientConn) <- setupBTClient $ btClientConfig config

  -- setup the files on which the client is working in a temp dir
  files <- setupContactFiles (contactFiles config) (fileCachePath config)
 
  -- setup proxies

 
  let onConn = serverConnInit
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


serverConnInit local remote = do
  return undefined

revProxy ip port = return $ ProxyAction {
                            command = CONNECT
                          , remoteAddr = (Right ip, port)
                          , onConnection = \ _ -> return () 
                          }


