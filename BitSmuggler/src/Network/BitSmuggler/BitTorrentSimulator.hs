{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}

module Network.BitSmuggler.BitTorrentSimulator where

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
 
import System.IO
import Control.Monad.Trans.Either
import Network.BitSmuggler.Utils


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

logger = "BitTorrentSimulator"

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


instance BERT () where
  showBERT () = NilTerm
  readBERT (NilTerm) =return ()

hoistBERTErrIO (Right v) = return v
hoistBERTErrIO (Left e) = case e of
  ClientError s -> throwIO BT.ClientException
  ServerError term -> throwIO BT.ServerException

clientConn :: String -> PortNum -> IO TorrentClientConn
clientConn url port = do
  t <- tcpClient url (PortNum . portNumFixEndian $ port)
  let rpc name params = call t rpcmod (show name) params >>= hoistBERTErrIO
  return $ TorrentClientConn {
      addMagnetLink = rpc AddMagnetOp . (:[])
    , addTorrentFile = rpc AddFileOp . (:[])
    , listTorrents = rpc ListOp ([] :: [Int]) -- type qualifier means nothing
    , pauseTorrent = rpc PauseOp . (:[])
    , removeTorrent  = rpc PauseOp . (:[])
    , setSettings = \ss -> return () -- no implementation
    , connectToPeer = Nothing
  }


type TorrentFileID = Either String InfoHash
data Config = Config {
    resourceMap :: Map.Map TorrentFileID ConnectionData
  , rpcPort :: Word16
  , peerPort :: Word16
} deriving (Show)

runClient conf = do
  tchan <- newTChanIO
  ts <- newTVarIO (Map.fromList [(torrentID fooTorrent, (fooTorrent, tchan))])
  let th = TorrentHandler ts 
                       (fromJust . (\k -> Map.lookup k (resourceMap conf)))
  concurrently (runServer th (rpcPort conf)) (listenForPeers $ (peerPort conf))

listenForPeers port = do
  runTCPServer (serverSettings (fromIntegral port) "*") $ \appData -> do
    debugM logger "received connection. draining socket.."
    (appSource appData) $$ awaitForever return

-- command server (rpc interface to send commands to the client)
runServer th port = do
 s <- tcpServer (PortNum . portNumFixEndian $ port)
 serve s $ dispatch th 

dispatch th "bittorrent" op params = case (read op, params) of
  (AddMagnetOp, [m]) -> do
    let (Right n) = readBERT m
    newTorrent th (Right $ n)
    return $ Success $ showBERT ()
  (AddFileOp, [fname]) -> do
    let (Right n) = readBERT fname
    newTorrent th (Left $ n)
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
      atomically $ writeTChan chan msg
      return . Success . showBERT $ ()
    Nothing -> return $ Undesignated "non-existant infohash"

fooTorrent = Torrent (Bin.decode $ BSL.replicate 20 1) "wtf.txt"

newTorrent th source = do
  let connData = resource th source
  let t = connTorrent connData
  cmdChan <- newTChanIO 
  atomically $ modifyTVar (torrents th) (Map.insert (torrentID t) (t, cmdChan))
  forkIO $ do
    asyncTorrent <- async $ doTorrent cmdChan connData
    final <- waitCatch $ asyncTorrent
    atomically $ modifyTVar (torrents th) (Map.delete (torrentID t))


doTorrent cmdChan connData = do
  let cd = connData
  runProxyTCPClient (BSC.pack . fst . proxyAddr $ cd) (snd . proxyAddr $ cd)
    (Socks4.clientProtocol (read . fst . peerAddr $ cd, snd . peerAddr $ cd) CONNECT)
    $ \sink resSrc remoteConn -> do
      -- TODO: now sending the file contents in a loop; this doesn't really work
      -- bc initial handshake gets repeated; change this
      withFile (dataFile connData) ReadMode $ \file -> forever $ do
        sourceHandle file $= conduitGet (DS.get :: DS.Get NetworkChunk)
          $= emitChunks cmdChan $$ sink
      return ()

data NetworkChunk = NetworkChunk BS.ByteString

instance Serialize NetworkChunk where
  get = do
    len <- getWord32be
    content <- getBytes (fromIntegral len)
    return $ NetworkChunk content
  put (NetworkChunk bs) = putWord32be (fromIntegral $ BS.length bs) >> putByteString bs

emitChunks cmdChan = go >> return () where
  go = runEitherT $ forever $ do
    maybeCmd <- liftIO $ atomically $ tryReadTChan cmdChan
    case maybeCmd of
      Just cmd -> takeCmd cmdChan cmd 
      Nothing -> return () -- keep going
    upstreamChunk <- lift $ await
    case upstreamChunk of 
      Just (NetworkChunk bs) -> (lift $ DC.yield bs)
      Nothing -> exit () 


takeCmd cmdChan cmd = case cmd of
  Stop -> exit () -- finish up
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
  , resource :: TorrentFileID -> ConnectionData
}


