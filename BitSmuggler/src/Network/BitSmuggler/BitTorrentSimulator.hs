{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Network.BitSmuggler.BitTorrentSimulator (
    runClient  
  , Config (..)
  , TorrentFileID
  , simulatorProc
  , clientConn
  , NetworkChunk (..)
  , ConnectionData (..)
  )where

import Prelude as P
import Network.BitTorrent.ClientControl as BT
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Binary as Bin
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Data.Map.Strict as Map
import Data.Serialize as DS
import Control.Monad.IO.Class
import Network.BERT.Server
import Network.BERT.Client
import Data.BERT.Types
import Data.BERT.Term
import Network.Socket
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Data.Conduit as DC
import Data.Conduit.Cereal
import Data.Conduit.List as CL
import Data.Maybe
import Network.TCP.Proxy.Client
import Network.TCP.Proxy.Socks4 as Socks4
import Data.Conduit.Binary
import Data.Conduit.Network
import Control.Monad
import System.Log.Logger
import qualified Data.Torrent as T
 
import System.IO
import Control.Monad.Trans.Either
import Network.BitSmuggler.Utils
import qualified Network.BitSmuggler.TorrentClientProc as Proc

{-
  simulates the network activity of a real bittorrent client
  built for testing purposes
 
  Features:
    * web api for taking commands
    * traffic replay
    * proxy usage
    * initiates connections; receives connections
  lacks:
    tracker/dht traffic immitation

   NON-DEFENSIVE programming - this code crashes if not given right input - it's only used for testing to it needen't be robust    
-}

-- CONSTANTS 

-- wait time between 2 bittorrent messages 
msgTimeInterval = 10 ^ 5 -- micro seconds


-- RPC aspects
rpcmod = "bittorrent"

data OpCode = AddMagnetOp | AddFileOp | ListOp | PauseOp | StopTorrentOp | SettingsOp
  deriving (Show, Eq, Read)

instance BERT InfoHash where
  showBERT = BinaryTerm . Bin.encode 
  readBERT (BinaryTerm ih) = return . Bin.decode $ ih  -- may throw exception

instance BERT Torrent where
  showBERT (Torrent id name)
    = DictionaryTerm [(AtomTerm "infohash", showBERT id), (AtomTerm "name", AtomTerm name)]
  readBERT (DictionaryTerm [(AtomTerm "infohash", ih), (AtomTerm "name", AtomTerm n)])
    = readBERT ih >>= (\ih -> return $ Torrent ih n)

-- WARNING: disgusting hack
-- put in a value that's never going to show up
nothin = AtomTerm "2452qtxiv3940ylesh430uih59nlt"

instance BERT () where
  showBERT () = nothin 
  readBERT t = if t == nothin then return () else (Left "fayul")

hoistBERTIO (Right v) = return v
hoistBERTIO (Left e) = case e of
  ClientError s -> throwIO BT.ClientException
  ServerError term -> throwIO BT.ServerException

clientConn :: String -> PortNum -> IO TorrentClientConn
clientConn url port = do
  t <- tcpClient url (PortNum . portNumFixEndian $ port)
  let rpc name params = call t rpcmod (show name) params >>= hoistBERTIO
  return $ TorrentClientConn {
      addMagnetLink = rpc AddMagnetOp . (:[])
    , addTorrentFile = rpc AddFileOp . (:[])
    , listTorrents = rpc ListOp ([] :: [Int]) -- type qualifier means nothing
    , pauseTorrent = rpc PauseOp . (:[])
    , removeTorrent  = rpc StopTorrentOp . (:[])
    , setSettings = \ss -> return () -- no implementation
    , connectToPeer = Nothing
  }


simulatorProc root resMap respSrc = do
  settingsVar <- newTVarIO (Nothing, Nothing)
  return $ Proc.TorrentProc {
    Proc.cleanState = return ()
  , Proc.start = do
    (Just rpc, Just peer)  <- atomically $ readTVar settingsVar
    forkIO $ (runClient $ Config {resourceMap = resMap, rpcPort = rpc, peerPort = peer
                         , respSource = respSrc}) >> return ()
    return ()
  , Proc.setSettings = \ss -> do
      s <- return $ case ss of 
        [Proc.CmdPort c, Proc.BindPort b] -> (Just c, Just b)
        [Proc.BindPort b, Proc.CmdPort c] -> (Just c, Just b)
      atomically $ writeTVar settingsVar s

  , Proc.getFilePath = \f ->  root P.++ "/" P.++ f
}


type TorrentFileID = Either String InfoHash
data Config = Config {
    resourceMap :: Map.Map TorrentFileID ConnectionData
  , rpcPort :: Word16
  , peerPort :: Word16
  , respSource :: String
} deriving (Show)

runClient conf = do
  tchan <- newTChanIO
  ts <- newTVarIO Map.empty -- (Map.fromList [(torrentID fooTorrent, (fooTorrent, tchan))])
  debugM logger "running bittorrent client simulator.."

  let th = TorrentHandler ts 
                       (\k -> Map.lookup k (resourceMap conf))
  concurrently (runServer th (rpcPort conf)) (listenForPeers (respSource conf) $ peerPort conf)

listenForPeers respSource port = do
  debugM logger "listening for incoming peer connections.."

  runTCPServer (serverSettings (fromIntegral port) "*") $ \appData -> do
    debugM logger "received connection. draining socket.."
    cmdChan <- newTChanIO 

    concurrently  (dumpChunkFile respSource (appSink appData) cmdChan Never)
                  ((appSource appData) $$ awaitForever return)
    return ()

-- command server (rpc interface to send commands to the client)
runServer th port = do
  s <- tcpServer (PortNum . portNumFixEndian $ port)
  debugM logger "running command server.."
 
  serve s $ dispatch th 

dispatch th "bittorrent" op params = case (read op, params) of
  (AddMagnetOp, [m]) -> do
    let (Right n) = readBERT m
    newTorrent th (Right $ n)
    return $ Success $ showBERT ()
  (AddFileOp, [fname]) -> do
    let (Right n) = readBERT fname
    debugM logger $ "adding file..." P.++ n
    newTorrent th (Left $ n)
    debugM logger "succesfully added file..."
    return $ Success $ showBERT ()
  (ListOp, []) -> do
    fmap (Success . showBERT . P.map (fst . snd) . Map.toList)
          $ atomically $ readTVar $ torrents th
  (PauseOp, [ihTerm]) -> messageTorrent th Pause ihTerm
  (StopTorrentOp, [ihTerm]) -> messageTorrent th Stop ihTerm
dispatch th _ _ _ =
  return NoSuchModule

messageTorrent th msg ihTerm = do
  let (Right ih) = readBERT ihTerm
  torrentProc <- fmap (Map.lookup ih) $ atomically $ readTVar $ torrents th
  case torrentProc of
    Just (t, chan) -> do 
      debugM logger "messaging torrent to stop"
      atomically $ writeTChan chan msg
      return . Success . showBERT $ ()
    Nothing -> return $ Undesignated "non-existant infohash"

fooTorrent = Torrent (Bin.decode $ BSL.replicate 20 1) "wtf.txt"

newTorrent th source = do
  debugM logger $ "adding a new torrent with source " P.++ (show source)
  realSource <- case source of
    Left torrentFilePath -> do
      tFile <- (fromRight . T.readTorrent . toLazy) <$> BS.readFile torrentFilePath
      return $ Left $ BSLC.unpack $ T.tName $ T.tInfo tFile  
    Right v -> return $ Right v
  case resource th realSource of
    Nothing -> do
      debugM logger $ "torrent data not present. not streaming anything"
     
    Just connData -> do
      let t = connTorrent connData
      debugM logger $ "torrent data is present. starting streaming.."
      cmdChan <- newTChanIO 

      atomically $ modifyTVar (torrents th) (Map.insert (torrentID t) (t, cmdChan))
      debugM logger "updated torrents list"
      forkIO $ do
        asyncTorrent <- async $ doTorrent cmdChan connData
        final <- waitCatch $ asyncTorrent
        
        debugM logger $ "torrent finished with " P.++ show final P.++ 
                         " deleting " P.++ (show $ torrentID t)

        atomically $ modifyTVar (torrents th) (Map.delete (torrentID t))
      return ()


doTorrent cmdChan connData = do
  let cd = connData
  debugM logger $ "attempting connection to " P.++ (show $ peerAddr cd) P.++
                  " through the proxy with address " P.++ (show $ proxyAddr cd)

  -- emulating disconnects
  forM [AfterNMsgs 40, Never] $ \disconnect -> do
    runProxyTCPClient (BSC.pack . fst . proxyAddr $ cd) (snd . proxyAddr $ cd)
      (Socks4.clientProtocol (read . fst . peerAddr $ cd, snd . peerAddr $ cd) CONNECT)
      $ \sink resSrc remoteConn -> do
        dumpChunkFile (dataFile connData) sink cmdChan disconnect
        return ()
    threadDelay $ 10 ^ 6


dumpChunkFile file sink cmdChan disconnect = do
  withFile file ReadMode $ \file -> do
    debugM logger $ "dumping traffic replay into socket from file "
                  P.++ (show file)
    sourceHandle file $= conduitGet (DS.get :: DS.Get NetworkChunk)
      $= emitChunks cmdChan $= (connCutter disconnect) $$ sink
    debugM logger $ "finished dumping"


data DisconnectEvent  = Never | AfterNMsgs Int
connCutter Never = CL.map P.id
-- cut one time after msgs messages
connCutter (AfterNMsgs msgs) = go 0
  where
    go n = do
      if n == msgs then return () -- terminate
      else do
        upstream <- await
        case upstream of
          (Just item) -> DC.yield item >> go (n + 1)
          Nothing -> return ()
      
  

data NetworkChunk = NetworkChunk BS.ByteString

instance Serialize NetworkChunk where
  get = do
    len <- getWord32be
    content <- getBytes (fromIntegral len)
    return $ NetworkChunk content
  put (NetworkChunk bs) = putWord32be (fromIntegral $ BS.length bs) >> putByteString bs

emitChunks cmdChan = go >> return () where
  go = runEitherT $ forever $ do
    liftIO $ threadDelay $ msgTimeInterval
    maybeCmd <- liftIO $ atomically $ tryReadTChan cmdChan
    case maybeCmd of
      Just cmd -> takeCmd cmdChan cmd 
      Nothing -> return () -- keep going
    upstreamChunk <- lift $ await
    case upstreamChunk of 
      Just (NetworkChunk bs) -> (lift $ DC.yield bs)
      Nothing -> exit () 


takeCmd cmdChan cmd = case cmd of
  Stop -> (liftIO $ debugM logger "exiting the torrent...") >> exit () -- finish up
  -- on pause it performs a blocking read on the cmd channel til other
  -- commands are received
  Pause -> (liftIO $ atomically $ readTChan cmdChan) >>= takeCmd cmdChan
  Resume -> return () -- proceed 

exit = left


data Cmd = Stop | Pause | Resume deriving (Eq, Show)

data ConnectionData = ConnectionData {
                                       connTorrent :: Torrent
                                     , dataFile :: FilePath
                                     , peerAddr :: (String, Word16)
                                     , proxyAddr :: (String, Word16)}
  deriving (Show)


data TorrentHandler = TorrentHandler {
    torrents :: TVar (Map InfoHash (Torrent, TChan Cmd))
  , resource :: TorrentFileID -> Maybe ConnectionData
}


testHash = Bin.decode $ BSL.replicate 20 0
testDataFile = "/home/dan/repos/bitSmuggler/bit-smuggler/testdata/smallCapture0"

testTFile = "/home/dan/repos/bitSmuggler/bit-smuggler/testdata/noxexistant.torrent"


testResourceMap = Map.fromList [(Left testTFile, testConnData)]

initiatorConf = Network.BitSmuggler.BitTorrentSimulator.Config {resourceMap = testResourceMap, rpcPort = 2015, peerPort = 3001, respSource = testDataFile}

testConnData = ConnectionData {connTorrent = Torrent  testHash "this is the filename"
                              , dataFile = testDataFile
                              , peerAddr = (localhost, peerPort receiverConf)
                              , proxyAddr = (localhost, 1080)}


receiverConf = Network.BitSmuggler.BitTorrentSimulator.Config {
                      resourceMap = testResourceMap
                      , rpcPort = 2016, peerPort = 3002, respSource = testDataFile}


initiatorPeer = do
  updateGlobalLogger logger  (setLevel DEBUG)
  runClient initiatorConf

receiverPeer = do
  updateGlobalLogger logger  (setLevel DEBUG)
  runClient receiverConf

tellStop = do
  c <- clientConn localhost (rpcPort initiatorConf)
  removeTorrent c testHash

tellInitiator = do
  updateGlobalLogger logger  (setLevel DEBUG)
  c <- clientConn localhost (rpcPort initiatorConf)
  addTorrentFile c testTFile
  lr <- listTorrents c
  debugM logger (show lr)
  P.getLine 
  removeTorrent c testHash

  
