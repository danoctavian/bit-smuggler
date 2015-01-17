{-# LANGUAGE RecordWildCards #-}
module Network.BitSmuggler.Client where

import Prelude as P hiding (read)
import Control.Monad.Trans.Resource
import System.Log.Logger
import Control.Monad.IO.Class
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import System.Random
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Conduit as DC
import Data.Conduit.List as DC


import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature)

import Network.BitSmuggler.Common
import Network.BitSmuggler.Crypto as Crypto
import Network.BitSmuggler.Protocol
import Network.BitSmuggler.Utils
import Network.BitSmuggler.ARQ


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

data ClientStage = FirstConnect | Reconnect SessionToken

data ClientState = ClientState {
    stage :: ClientStage
  , currentInfoHash :: InfoHash
  , handlerTask :: Maybe (Async ())

  , pieceProxyTask :: Maybe (Async ())
}


clientConnect :: ClientConfig -> (ConnData -> IO ()) -> IO ()
clientConnect (ClientConfig {..}) handle = runResourceT $ do

  userSend <- liftIO $ (newTQueueIO :: IO (TQueue ServerMessage))
  userRecv <- liftIO $ (newTQueueIO :: IO (TQueue ClientMessage))

  liftIO $ debugM logger "starting client "

  -- start torrent client (with config)

  (btProc, btClientConn) <- setupBTClient $ btClientConfig

  -- setup the FILE on which the client is working
  -- choose it randomly from the set of contact files
  let possibleContacts = contactFiles $ serverDescriptor
  pick <- liftIO $ randInt (0, P.length possibleContacts - 1)
  let contactFile = possibleContacts !! pick

  [file] <- setupContactFiles [contactFile] fileCachePath

  cprg <- liftIO $ makeCPRG
  let (cryptoOps, pubKeyRepr) = makeClientEncryption (serverPubKey serverDescriptor) cprg
  pieceHooks <- liftIO $ makePieceHooks
    
  let onConn = undefined
  -- setup proxies (socks and reverse)
  (reverseProxy, forwardProxy) <- startProxies btClientConfig onConn
  
  -- tell client to start working on file 
  return ()


clientProxyInit stateVar serverAddress (PieceHooks {..}) (cryptoOps, repr) local remote = do
  if (remote == serverAddress)
  then do
    state <- atomically $ readTVar stateVar
    case stage state of 
      FirstConnect -> do
        cprg <- makeCPRG

        -- send the first message (hanshake)
        DC.sourceList [Just $ ConnRequest repr Nothing]
                   =$ sendPipe packetSize (sendARQ noARQ)
                        (encrypter (encryptHandshake (cryptoOps, repr)) cprg)
                   $$ outgoingSink (read sendGetPiece) 
                                   (\p -> write sendPutBack p)
       
         
        return ()

      Reconnect token -> do
        errorM logger "reconnect not implement at the moment"
        throwIO UnsupportedFeature
    return undefined    
    
  -- it's some other connection - just proxy data without any 
  -- parsing or tampering
  else return $ DataHooks { incoming = DC.map P.id
                          , outgoing = DC.map P.id
                          , onDisconnect = return () -- don't do anything
                        }


packetSize = blockSize - Crypto.msgHeaderLen
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
