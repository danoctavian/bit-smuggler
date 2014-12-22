import Prelude as P

import Network.TCP.Proxy.Server
import Data.Binary as Bin

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.Char8 as BSLC

import System.IO
import System.Random

import Network.TCP.Proxy.Client
import Network.TCP.Proxy.Socks4 as Socks4
import Network.TCP.Proxy.Server as Proxy
import Data.Map.Strict as Map
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import System.Log.Logger

import Data.Conduit.Binary
import Data.Conduit.List as CL
import Data.Conduit as DC

import Network.BitTorrent.ClientControl
import Network.BitSmuggler.BitTorrentSimulator as Sim
import Network.BitSmuggler.Utils
import Data.Serialize as DS
import Control.Concurrent

{-

project created to run experiments 
-}

main = do 
  P.putStrLn "running bit-smuggler script"

testHash = Bin.decode $ BSL.replicate 20 0
testDataFile = "/home/dan/repos/bitSmuggler/bit-smuggler/testdata/trafficSample00"

localhost = "127.0.0.1"

initiatorConf = Sim.Config {resourceMap = Map.empty, rpcPort = 2015, peerPort = 3001}

testConnData = ConnectionData {connTorrent = Torrent  testHash "this is the filename"
                              , dataFile = testDataFile
                              , peerAddr = (localhost, peerPort initiatorConf)
                              , proxyAddr = (localhost, 1080)}


receiverConf = Sim.Config {
                      resourceMap = Map.fromList [(Left testDataFile, testConnData)]
                      , rpcPort = 2016, peerPort = 3002}

initiatorPeer = do
  Sim.runClient initiatorConf

receiverPeer = do
  Sim.runClient receiverConf

initCaptureHook incFile outFile a1 a2 = do
  incHook <- captureHook incFile
  outHook <- captureHook outFile
  return $ DataHooks incHook outHook 

captureHook :: FilePath -> IO (BS.ByteString -> IO BS.ByteString)
captureHook file = do
  rgen <- newStdGen
  let r = (fst $ random rgen) :: Int
  tchan <- newTChanIO 
  debugM logger $ "setting up capture for " P.++ file
  forkIO $ do
    withFile (file P.++ (show r)) WriteMode $ \fileH -> do
      sourceTChan tchan =$ (CL.map (DS.encode . NetworkChunk))  $$ sinkHandle fileH
  debugM logger $ "done setting up capture"
 
  return $ \bs -> atomically $ writeTChan tchan bs >> return bs
 

trafficCapture = do
  updateGlobalLogger logger  (setLevel DEBUG)
  Proxy.run $ Proxy.Config { proxyPort = 1080
            , initHook =  initCaptureHook "incomingCapture" "outgoingCapture"
            , handshake = Socks4.serverProtocol
       }


printChunk bs = debugM logger (show $ BS.length bs) >> return bs

trafficProxy = do
  updateGlobalLogger logger  (setLevel DEBUG)
  Proxy.run $ Proxy.Config { proxyPort = 1080
            , initHook = (\_ _ -> return $ DataHooks printChunk printChunk) 
            , handshake = Socks4.serverProtocol
       }

 
