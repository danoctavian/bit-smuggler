{-# LANGUAGE RecordWildCards #-}
module Network.BitSmuggler.Client where

import Prelude as P hiding (read)
import qualified Data.Tuple as Tup
import Control.Monad.Trans.Resource
import System.Log.Logger
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import System.Random
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Conduit as DC
import Data.Conduit.List as DC


import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature)

import Network.BitSmuggler.Common hiding (logger)
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
    serverToken :: Maybe SessionToken
--  , currentInfoHash :: InfoHash
}


clientConnect :: ClientConfig -> (ConnData -> IO ()) -> IO ()
clientConnect (ClientConfig {..}) handle = runResourceT $ do

  liftIO $ debugM logger "starting client "

  -- start torrent client (with config)
  (btProc, btClientConn) <- setupBTClient $ btClientConfig

  -- setup the FILE on which the client is working
  -- choose it randomly from the set of contact files
  let possibleContacts = contactFiles $ serverDescriptor
  pick <- liftIO $ randInt (0, P.length possibleContacts - 1)
  let contactFile = possibleContacts !! pick

  [file] <- setupContactFiles [contactFile] fileCachePath


  -- == CLIENT INITIALIZATION =
  cprg <- liftIO $ makeCPRG
  let (cryptoOps, pubKeyRepr) = makeClientEncryption (serverPubKey serverDescriptor) cprg
  encryptCprg <- liftIO $ makeCPRG
  let clientEncrypter = encrypter (encrypt cryptoOps) encryptCprg

  pieceHooks <- liftIO $ makePieceHooks

  controlSend <-liftIO $ (newTQueueIO :: IO (TQueue ClientMessage))
  controlRecv <- liftIO $ (newTQueueIO :: IO (TQueue ServerMessage))
  let controlPipe = Pipe controlRecv controlSend

  dataGate <- liftIO $ newGate
  let dataPipes = DataPipes controlPipe pieceHooks dataGate
  
  userPipe <- launchPipes packetSize noARQ clientEncrypter (decrypt cryptoOps)  dataPipes
  userGate <- liftIO $ newGate -- closed gate
  exitGate <- liftIO $ newGate
  -- the provided user handle. runs only when connection started
  -- (gate opens)
  allocLinkedAsync $ async $ do
     atomically $ goThroughGate userGate
     handle $ pipeToConnData userPipe
     atomically $ openGate exitGate -- signal termination


  clientState <- liftIO $ newTVarIO $ ClientState Nothing 

  let fileFixer = findPieceLoader [file]
 
  let handleConn = handleConnection clientState
                    (cryptoOps, pubKeyRepr) userPipe userGate dataPipes
  let onConn = clientProxyInit handleConn pieceHooks fileFixer (serverAddr serverDescriptor)

  liftIO $ debugM logger "finished initializng client..."

  -- setup proxies (socks and reverse)
  (reverseProxy, forwardProxy) <- startProxies btClientConfig onConn
  
  -- tell client to start working on file 

  liftIO $ debugM logger "adding files to bittorrent client..."
  liftIO $ addTorrents btClientConn (fst btProc) [file]

  liftIO $ debugM logger "waiting for user handle to execute."
  liftIO $ atomically $ goThroughGate exitGate
  liftIO $ debugM logger "client is terminated."

  return ()


clientProxyInit handleConn pieceHs fileFix serverAddress direction local remote = do

  liftIO $ debugM logger $ "bittorrent client connects to remote " P.++ (show remote)

  if (fst remote == serverAddress)
  then do
    forkIO $ handleConn
    streams <- fmap (if direction == Reverse then Tup.swap else P.id) $
                    makeStreams pieceHs fileFix
    return $ DataHooks { incoming = P.fst streams
                         , outgoing = P.snd streams 
                         , onDisconnect = return () -- TODO: implement 
                        }

  -- it's some other connection - just proxy data without any 
  -- parsing or tampering
  else return $ Proxy.DataHooks { incoming = DC.map P.id
                          , outgoing = DC.map P.id
                          , onDisconnect = return () -- don't do anything
                        }


packetSize = blockSize - Crypto.msgHeaderLen

handleConnection stateVar  (cryptoOps, repr) userPipe userGate
  (DataPipes control (PieceHooks {..}) dataGate) = do
  state <- atomically $ readTVar stateVar

  cprg <- makeCPRG
  let prevToken = serverToken state

  noGate <- newGate
  atomically $ openGate noGate
  -- send the first message (hanshake)
  DC.sourceList [Just $ ConnRequest repr (serverToken state)]
             =$ sendPipe packetSize (sendARQ noARQ)
                  (encrypter (encryptHandshake (cryptoOps, repr)) cprg)
             $$ outgoingSink (read sendGetPiece) 
                             (\p -> write sendPutBack p) noGate

  if (prevToken == Nothing) then do -- first time connecting
    serverResponse <- liftIO $ atomically $ readTQueue (pipeRecv control)
    case serverResponse of
      AcceptConn token -> do
        liftIO $ debugM logger $ "connection to server succesful "

        atomically $ modifyTVar stateVar (\s -> s {serverToken = Just token}) 
        atomically $ openGate userGate -- start the user function
      RejectConn -> do
        errorM logger "connection rejected"
        -- TODO: clean up conn
        return ()
  else do
    infoM logger "it's a reconnect. nothing to do..."
    atomically $ openGate dataGate -- allow messages to pass
    
  return ()

randInt :: (Int, Int) ->  IO Int 
randInt range = getStdGen >>= return . fst . (randomR range)
