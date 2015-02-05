{-# LANGUAGE OverloadedStrings #-}
module Network.BitSmuggler.IntegrationSpec where

import Prelude as P
import Data.Torrent
import Data.Maybe
import Data.Text as T
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import System.FilePath.Posix
import Crypto.Random
import Crypto.Curve25519
import Data.Byteable
import Crypto.Random.AESCtr as AESCtr
import Data.IP
import Data.Serialize as DS
import Data.Map.Strict as Map
import Data.Binary as Bin
import qualified Network.BitTorrent.ClientControl as BT
import qualified Network.BitTorrent.ClientControl.UTorrent as UT
import System.IO
import System.Log.Logger
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Control.Exception.Base
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class

import qualified Network.BitTorrent.Shepherd as Tracker


import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature, logger)
import Network.TCP.Proxy.Socks4 as Socks4


import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.Conduit.Binary as CBin
import Data.Conduit.Network

import Network.BitSmuggler.Common as Common
import Network.BitSmuggler.Common as Protocol
import Network.BitSmuggler.Utils
import Network.BitSmuggler.TorrentFile
import Network.BitSmuggler.Crypto as Crypto
import Network.BitSmuggler.Server as Server
import Network.BitSmuggler.Client as Client
import Network.BitSmuggler.FileCache as Cache
import Network.BitSmuggler.TorrentClientProc as Proc
import Network.BitSmuggler.Protocol

{-

Integration test for bitsmuggler with 1 server and 1 client
both running on the same machine

TODO: move to the test directory after stabilizing the code
currently wrote in a normal module
-}


testRoot = "test-data/integration-test/"


data TestFile = TestFile {metadata :: (FilePath, Text, Int), fileDataPath :: String}
--2 gb file?

bigFile = TestFile bigTestFile bigTestDataFile
bigTestFile = (testRoot </> "contactFile/testFileBig.torrent"
                , "ef967fc9d342a4ba5c4604c7b9f7b28e9e740b2f"
                , 69)

bigTestDataFile = testRoot </> "contactFile/testFileBig.txt"


-- 100 mb file
smallFile = TestFile smallTestFile smallTestDataFile
smallTestFile = (testRoot </> "contactFile/testFile.torrent"
                , "f921dd6548298527d40757fb264de07f7a47767f"
                , 23456)
smallTestDataFile = testRoot </> "contactFile/testFile.txt"

makePaths prefix = P.map ((testRoot </> prefix) </> ) ["cache", "utorrent-client"]

localhostIP = IPv4 $ toIPv4 [127,0,0,1]

runIntegrationTest :: IO ()
runIntegrationTest = runResourceT $ do
  let testFile = bigFile

  liftIO $ updateGlobalLogger logger  (setLevel DEBUG)
  liftIO $ updateGlobalLogger Tracker.logger  (setLevel DEBUG)

  liftIO $ debugM logger "running integration test"

  let [serverCache, serverUTClientPath] = makePaths "server"
  let [clientCache, clientUTClientPath] = makePaths "client"

  contact <- liftIO $ makeContactFile (metadata testFile)
  (serverDesc, serverSk)
    <- liftIO $ makeServerDescriptor contact localhostIP

  -- launch the tracker
  trackEvents <- liftIO $ newTChanIO
  tracker <- allocAsync $ async $ Tracker.runTracker
                      $ Tracker.Config { Tracker.listenPort = 6666
                                         , Tracker.events = Just trackEvents}
  liftIO $ waitFor (== Tracker.Booting) trackEvents

  allocAsync $ async $ runServer serverUTClientPath serverCache contact
                                       (serverDesc, serverSk)

  liftIO $ debugM logger "booted server.."

--  liftIO $ threadDelay $ 10 ^ 9

  liftIO $ waitFor (\(Tracker.AnnounceEv a) -> True) trackEvents
  liftIO $ debugM logger "tracker got announce from the server"

  liftIO $ debugM logger "running client now"

  allocAsync $ async $ runClient clientUTClientPath clientCache serverDesc

  liftIO $ threadDelay $ 10 ^ 9
  
  return ()


 -- UTORRENT based client and server 
runClient torrentProcPath cachePath serverDesc = do
  proc <- uTorrentProc torrentProcPath

  let btC = clientBTClientConfig {
                btProc = proc
              , outgoingRedirects
                             = redirectToRev (serverAddr serverDesc) serverBTClientConfig
              }

  Client.clientConnect (ClientConfig btC serverDesc cachePath) clientChunkExchange
  return ()

runServer torrentProcPath cachePath contact (serverDesc, serverSk) = do

  proc <- uTorrentProc torrentProcPath
  let btC = serverBTClientConfig {
                 btProc = proc
               , outgoingRedirects
                  = redirectToRev (serverAddr serverDesc) clientBTClientConfig 
               }

  Server.listen (ServerConfig serverSk btC [contact] cachePath) serverChunkExchange
  return ()

-- were are configuring the proxies to redirect the bittorrent traffic
-- to the reverse proxy port
-- so that we don't need to play with iptables
redirectToRev ip conf = Map.fromList
   [((Right ip, pubBitTorrentPort conf),(Right ip, revProxyPort conf))]


chunks = [ BS.replicate 1000 99, BS.replicate (10 ^ 4)  200
         , BS.concat [BS.replicate (10 ^ 4) 39, BS.replicate (10 ^ 4) 40]
         , BS.replicate (10 ^ 4)  173
         , BS.replicate (10 ^ 3)  201
         , BS.replicate (10 ^ 3)  202] P.++ smallChunks

smallChunks = P.map (BS.replicate (10 ^ 2)) [1..5]

serverChunkExchange c = do
  infoM logger "USER: waiting for signs of life from client"
  message <- connRecv c
  P.putStrLn $ show message
  connSend c "hello from server"
  forM (P.zip chunks [1..]) $ \(chunk, i) -> do
    bigBlock <- connRecv c
    assert (bigBlock == chunk) (return ())
    debugM logger $ "server received big chunk succesfully " P.++ (show i)

    connSend c chunk
  threadDelay $ 10 ^ 8


clientChunkExchange c = do
  infoM logger "USER: sending the server a hello"
  connSend c "hello from client"
  response <- connRecv c
  infoM logger $ show response
  forM (P.zip chunks [1..]) $ \(chunk, i) -> do
    connSend c chunk
    bigBlock <- connRecv c
    assert (bigBlock == chunk) (return ())
    debugM logger $ "client received big chunk succesfully " P.++ (show i)
  threadDelay $ 10 ^ 8


makeContactFile (filePath, infoHash, seed) = do
  Right t <- fmap readTorrent $ BSL.readFile $ filePath
  return $ FakeFile {seed = seed, torrentFile = t
                    , infoHash = fromRight $ DS.decode $ fromJust $ textToInfoHash infoHash}

makeServerDescriptor contact ip = do
  let cprg = cprgCreate $ createTestEntropyPool "leSeed" :: AESRNG
  let (skBytes, next2) = cprgGenerate Crypto.keySize cprg
  let serverSkWord = (fromRight $ DS.decode skBytes :: Key)
  let serverPk = derivePublicKey (fromBytes $ toBytes serverSkWord)
  let serverPkWord = (fromRight $ DS.decode (toBytes serverPk) :: Key)

  return $ (ServerDescriptor ip [contact] serverPkWord
            , serverSkWord)


initIntegrationTestCaches testFile = do
  let serverCache = P.head $ makePaths "server"
  let clientCache = P.head $ makePaths "client"
  initFileCache serverCache  testFile
  initFileCache clientCache testFile


initFileCache cachePath testFile = do
  let (tpath, ih, seed)  = metadata testFile
  fHandle <- openFile (fileDataPath testFile) ReadMode
  cache <- Cache.load cachePath  
  Cache.put cache (fromRight $ DS.decode $ fromJust $ textToInfoHash ih :: InfoHash)
                  $  sourceHandle fHandle
  hClose fHandle
  Cache.close cache


uTorrentConnect host port = UT.makeUTorrentConn host port ("admin", "")

waitFor cond chan = do
  n <- atomically $ readTChan chan
  if (cond n) then return n
  else waitFor cond chan


clientBTClientConfig = BTClientConfig {
    pubBitTorrentPort = 5881
  , socksProxyPort = 2001
  , revProxyPort = 2002
  , cmdPort = 8000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = uTorrentConnect
}

serverBTClientConfig = BTClientConfig {
    pubBitTorrentPort = 7881
  , socksProxyPort = 3001
  , revProxyPort = 3002
  , cmdPort = 9000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = uTorrentConnect
}
