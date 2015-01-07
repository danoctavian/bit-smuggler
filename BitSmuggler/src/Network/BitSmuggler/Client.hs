module Network.BitSmuggler.Client where

import Network.BitSmuggler.Common
import Network.BitSmuggler.Crypto (Key)

import System.Log.Logger
{-

TODO: implement

-}

logger = "BitSmuggler.Client"

data ClientConfig = ClientConfig {
    btClientConfig :: BTClientConfig
  , serverDescriptor :: ServerDescriptor
}

clientConnect :: ClientConfig -> (ConnData -> IO ()) -> IO ()
clientConnect config handle = do
  debugM logger "starting client "

  -- start torrent client (with config)

  -- connect to it
  -- configure it

  -- setup the FILE on which the client is working

  -- setup proxies (socks and reverse)
  
  -- tell client to start working on file 
  
