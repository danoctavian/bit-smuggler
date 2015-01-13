module Network.BitSmuggler.Client where

import Prelude as P            
import Control.Monad.Trans.Resource
import System.Log.Logger
import Control.Monad.IO.Class
import System.Random

import Network.BitSmuggler.Common
import Network.BitSmuggler.Crypto (Key)

{-

client is designed to support 1 single connection
at the moment. this simplifies the code for now.

Client and server are quite similar. in the end they can
be written as peers, not clients and servers, and any 
peer can make and recieve connection.

for now we stick with this design to cover the proxy 
server usecase. 

A proxy server could just be a peer with more bandwitdth
and a better a machine to back it up.
-}

logger = "BitSmuggler.Client"

data ClientConfig = ClientConfig {
    btClientConfig :: BTClientConfig
  , serverDescriptor :: ServerDescriptor
  , fileCachePath :: FilePath
}

clientConnect :: ClientConfig -> (ConnData -> IO ()) -> IO ()
clientConnect config handle = runResourceT $ do
  liftIO $ debugM logger "starting client "

  -- start torrent client (with config)

  (btProc, btClientConn) <- setupBTClient $ btClientConfig config

  -- setup the FILE on which the client is working
  let possibleContacts = contactFiles $ serverDescriptor config
  pick <- liftIO $ randInt (0, P.length possibleContacts - 1)
  let contactFile = possibleContacts !! pick

  [file] <- setupContactFiles [contactFile] (fileCachePath config)

  -- setup proxies (socks and reverse)
  
  -- tell client to start working on file 
  return ()

{-

  before proxy init  -
   setup chans for bittorrent stream
   setup chans for userdata 
    pass in the bt stream chans to proxy_init
  proxies init
 check if remote is desired server. if not, just proxy traffic
  without doing anything else.

  if it is, just place in the hooks
-}

randInt :: (Int, Int) ->  IO Int 
randInt range = getStdGen >>= return . fst . (randomR range)
