{-# LANGUAGE OverloadedStrings #-}
module Network.BitSmuggler.DemoSetup where

import Prelude as P
import Data.Torrent
import Data.Maybe
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
import Control.Exception.Base
import Control.Monad

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
import Network.BitSmuggler.BitTorrentSimulator as Sim
import Network.BitSmuggler.Server as Server
import Network.BitSmuggler.Client as Client
import Network.BitSmuggler.FileCache as Cache
import Network.BitSmuggler.TorrentClientProc as Proc
import Network.BitSmuggler.Protocol


chunks = [ BS.replicate 1000 99, BS.replicate (10 ^ 4)  200
         , BS.concat [BS.replicate (10 ^ 4) 39, BS.replicate (10 ^ 4) 40]
         , BS.replicate (10 ^ 4)  173
         , BS.replicate (10 ^ 3)  201
         , BS.replicate (10 ^ 3)  202] P.++ smallChunks

smallChunks = P.map (BS.replicate (10 ^ 2)) [1..5]

-- =======================

torrentFilePath = "../contactFile/testFile.torrent"

torrentProcPath = "utorrent-client"
cachePath = "cache"

testServerIP = [5,151,211,150]

uTorrentConnect host port = UT.makeUTorrentConn host port ("admin", "")

tryUTorrentProc = do
  proc <- uTorrentProc "/home/dan/tools/bittorrent/utorrent-server-alpha-v3_3_1"
  Proc.cleanState proc 
  forkIO $ do
    threadDelay $ 10 ^ 6 * 3
    conn <- uTorrentConnect "127.0.0.1" 7999 
    BT.setSettings conn [BT.BindPort 2015]
  start proc
  

-- UTORRENT based client and server 
runRealDemoClient = do
  updateGlobalLogger logger  (setLevel DEBUG)
  contact <- makeContactFile torrentFilePath 
  (serverDesc, _) <- makeServerDescriptor contact (IPv4 $ toIPv4 testServerIP)

  proc <- uTorrentProc torrentProcPath

  let btC = clientBTClientConfig { btProc = proc
                                 , connectToClient = uTorrentConnect}
  Client.clientConnect (ClientConfig btC serverDesc cachePath) clientChunkExchange
  return ()

runRealDemoServer = do
  updateGlobalLogger logger  (setLevel DEBUG)
  contact <- makeContactFile torrentFilePath 
  (serverDesc, sk) <- makeServerDescriptor contact (IPv4 $ toIPv4 testServerIP)

  proc <- uTorrentProc torrentProcPath
  let btC = serverBTClientConfig { btProc = proc
                                 , connectToClient = uTorrentConnect}

  Server.listen (ServerConfig sk btC [contact] cachePath) serverChunkExchange
  return ()

-- =======================

-- SIMULATOR BASED client and server
setupFileCache path torrentFilePath dataFilePath = do
  contact <- makeContactFile torrentFilePath 
  fHandle <- openFile dataFilePath ReadMode
  cache <- Cache.load path 
  Cache.put cache (infoHash contact) $  sourceHandle fHandle
  hClose fHandle
  Cache.close cache


-- does it work ?
runFullDemo = do
  let trackerPort = 1337
  tracker <- async $ echoServer trackerPort
  server <- async $ runDemoServer
  threadDelay $ 10 ^ 6 * 5
  client <- async $ runDemoClient trackerPort

  waitBoth server client
  debugM logger "finished demo"
  return ()


runDemoClient trackerPort = do
  updateGlobalLogger logger  (setLevel DEBUG)

  contact <- makeContactFile testTFile

  debugM logger $ "the contact file's infohash is " P.++ (show $ infoHash contact)

  (serverDesc, _) <- makeServerDescriptor contact (IPv4 $ toIPv4 [127,0,0,1])

  let connData = ConnectionData {
              connTorrent = BT.Torrent  (infoHash contact) $ takeFileName testDataFile
            , dataFile = streamFile
              -- no need for iptables redirection: we tell the bittorrent simulator
              -- to connect straight to the reverse proxy
            , peerAddr = (localhost, revProxyPort serverBTClientConfig)
            , proxyAddr = (localhost, socksProxyPort clientBTClientConfig)
            , trackerAddr = (localhost, trackerPort)}

  proc <- simulatorProc clientBTRoot
           (Map.fromList [(Left $ takeFileName testDataFile, connData)])  streamFile

  let btC = clientBTClientConfig {btProc = proc}
  Client.clientConnect (ClientConfig btC serverDesc serverCachePath) clientChunkExchange 


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


runDemoServer = do
  updateGlobalLogger logger  (setLevel DEBUG)

  contact <- makeContactFile testTFile

  -- the stream has 1156258573216559082388624324876499110422886733169
  -- this has 1422293912806317530710726412509278710035024148095
  debugM logger $ "the contact file's infohash is " P.++ (show $ infoHash contact)

  (serverDesc, sk) <- makeServerDescriptor contact (IPv4 $ toIPv4 [127,0,0,1])

  proc <- simulatorProc serverBTRoot Map.empty streamFile

  let btC = serverBTClientConfig {btProc = proc}
  Server.listen (ServerConfig sk btC [contact] clientCachePath) serverChunkExchange

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


root = "/home/dan/repos/bitSmuggler/bit-smuggler/demo"

clientBTRoot = root </> "client/utorrent-client"
serverBTRoot = root </> "server/utorrent-client"


clientCachePath = root </> "client/cache"
serverCachePath = root </> "server/cache"


streamFile = root </> "contactFile/testFileLenPrefixedStream"
testTFile = root </> "contactFile/testFile.torrent"

testDataFile = root </> "contactFile/testFile.txt"

makeContactFile filePath = do
  Right t <- fmap readTorrent $ BSL.readFile $ filePath
  return $ FakeFile {seed = 23456, torrentFile = t
                    , infoHash = fromRight $ DS.decode $ fromJust $ textToInfoHash
                                  "f921dd6548298527d40757fb264de07f7a47767f"}
  --fromRight $ DS.decode $ computeInfoHash t}


makeServerDescriptor contact ip = do
  let cprg = cprgCreate $ createTestEntropyPool "leSeed" :: AESRNG
  let (skBytes, next2) = cprgGenerate Crypto.keySize cprg
  let serverSkWord = (fromRight $ DS.decode skBytes :: Key)
  let serverPk = derivePublicKey (fromBytes $ toBytes serverSkWord)
  let serverPkWord = (fromRight $ DS.decode (toBytes serverPk) :: Key)
  
  return $ (ServerDescriptor ip [contact] serverPkWord
            , serverSkWord)


clientBTClientConfig = BTClientConfig {
    pubBitTorrentPort = 5881
  , socksProxyPort = 2001
  , revProxyPort = 2002
  , cmdPort = 8000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = Sim.clientConn 
}

serverBTClientConfig = BTClientConfig {
    pubBitTorrentPort = 7881
  , socksProxyPort = 3001
  , revProxyPort = 3002
  , cmdPort = 9000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = Sim.clientConn 
}


----- try stream handler

tryBogus = do
  P.putStrLn "bogus" 
  withFile "../demo/contactFile/trackerCapture" ReadMode $ \h -> do
--    DC.sourceList ["omfg"]
    (sourceHandle h =$ CBin.isolate 302 >> DC.yield "junkie and shit innit")
      =$ btStreamHandler (DC.map id) $$ DC.consume

socketPing ip port = do
  runTCPClient (clientSettings port ip) $ \appData -> do
    let btHS = "\DC3BitTorrent protocol\NUL\NUL\NUL\NUL\NUL\DLE\NUL\ENQ\249!\221eH)\133'\212\aW\251&M\224\DELzGv\DEL-UT330B-\ACKwM\194+N\187\t14\240\136"
    let otherHS = "\DC3BitTorrent protocol\NUL\NUL\NUL\NUL\NUL\DLE\NUL\ENQ\249!\221eH)\133'\212\aW\251&M\224\DELzGv\DEL-UT330B-\ACKw\233'\186\EOT\210\152K\US;n"
    DC.sourceList [otherHS] $$ (appSink appData)
    debugM logger "sent out a protocol handshake. waiting for some reply..."
    -- take anything at all as a response
    forM [1..10] $ \_ -> do
      byteResp <- (appSource appData) $$ CBin.take 68
      debugM logger $ "resp is " ++ show byteResp
      threadDelay $ 10 ^ 6
    return ()

runPing = do
  updateGlobalLogger logger  (setLevel DEBUG)
  socketPing "127.0.0.1" 5881
   
    

