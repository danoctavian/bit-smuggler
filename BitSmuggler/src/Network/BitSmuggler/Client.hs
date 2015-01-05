module Network.BitSmuggler.Client where

import Network.BitSmuggler.Protocol
import System.Log.Logger
{-

TODO: implement

-}

logger = "BitSmuggler.Client"

data ClientConfig = ClientConfig {
    btClientConfig :: BTClientConfig
}


clientConnect :: ClientConfig -> (ConnData -> IO ()) -> IO ()
clientConnect config handle = do
  debugM logger "starting client "
