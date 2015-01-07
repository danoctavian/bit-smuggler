module Network.BitSmuggler.Server where

import Prelude as P
import Network.BitSmuggler.Crypto (Key)
import System.Log.Logger
import Data.Word
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString
import Control.Monad.IO.Class

import Network.BitSmuggler.Common
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
  , contactFiles :: ContactFile
}


listen :: ServerConfig -> (ConnData -> IO ()) -> IO ()
listen config handle = runResourceT $ do
  liftIO $ debugM logger "started bit-smuggler server..."


  -- start torrent client (with config)
  (btProc, btClientConn) <- setupBTClient $ btClientConfig config

  -- setup the files on which the client is working in a temp dir

  -- setup proxies (socks and reverse)

  -- tell client to use the files

  -- wait for it...
  -- in case of torrent app crash - restart it 
  return ()



