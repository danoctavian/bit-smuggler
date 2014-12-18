{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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
import Data.Map.Strict as Map
import Data.Serialize as DS

import Network.BERT.Server
import Network.BERT.Client
import Data.BERT.Types
import Data.BERT.Term
import Network.Socket
import Debug.Trace
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

import Network.TCP.Proxy.Client
import Network.TCP.Proxy.Socks4

{-
  simulates the network activity of a real bittorrent client
  built for testing purposes
 
  Features:
    * web api for taking commands
    * traffic replay
    * proxy usage
    * initiates connections; receives connections
  
    Implementation
    * best way for haskell rpc or rest or whatever that is
       -- messagepack haskell seems pretty easy -- ppl don't bump dependencies...
       -- bert - some random shit that actually works 
    * commands come in; pause/start whatever. how to do that with a worker thread?
       tchan that is read at each loop of its activity to check for commands
    * traffic replay: traffic is big - load it from file need chunks with their timestamps;
      can i really use the timestamps - what if my own falls behind - challenge make sure it doesn't? or just ignore timestamps and keep sending at quick pace == FOR NOW IGNORE TIMESTAMPTS - just send chunks in sequence 
    
    * lay it out as a conduit who reads from file and sends to a socket, while checking an stm chan for other tasks - pause, stop (throttle?), unpause
-}

{- rpc interface
addMagnetLink :: String -> IO ()
addTorrentFile :: FilePath -> IO ()
listTorrents :: IO [Torrent]
pauseTorrent :: InfoHash -> IO ()
stopTorrent :: InfoHash -> IO ()
setSettings :: [Setting] -> IO ()

 optional functionality
connectToPeer :: Maybe (InfoHash -> String -> PortNum -> IO ())

-}

-- RPC aspects

rpcmod = "bittorrent"

data OpCode = AddMagnetOp | AddFileOp | ListOp | PauseOp | StopTorrentOp | SettingsOp
  deriving (Show, Eq, Read)

data Op = AddMagnet InfoHash | AddFile String
            | PauseAction InfoHash | StopAction InfoHash | SettingsAction



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
  t <- tcpClient url (PortNum . portNumEndianness $ port)
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

-- server side
runServer th port = do
 s <- tcpServer (PortNum . portNumEndianness $ port)
 serve s $ dispatch th 

dispatch th "bittorrent" op params = case (read op, params) of
  (AddMagnetOp, [m]) -> do
    return $ Success $ showBERT ()
    
  (ListOp, []) -> do
    fmap (Success . showBERT . P.map (fst . snd) . Map.toList)
          $ atomically $ readTVar $ torrents th
  (PauseOp, [ihTerm]) -> messageTorrent th Pause ihTerm
  (StopTorrentOp, [ihTerm]) -> messageTorrent th Stop ihTerm
dispatch th "calc" _ _ =
  return NoSuchFunction
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
  return ()


data Cmd = Stop | Pause | Resume deriving (Eq, Show)

data ConnectionData = ConnectionData {
                                       connTorrent :: Torrent
                                     , dataFile :: FilePath
                                     , peerAddr :: String
                                     , peerPort :: Word16 }
  deriving (Show)


data TorrentHandler = TorrentHandler {
    torrents :: TVar (Map InfoHash (Torrent, TChan Cmd))
  , resource :: Either String InfoHash -> ConnectionData
}

portNumEndianness = (\(Right v) -> v) . runGet getWord16be  . runPut . putWord16host


-- DEUBG CODE
testClient = do
  c <- clientConn "127.0.0.1" 1100
  ts <- listTorrents c
  P.putStrLn $ show ts

testServer = do
  tchan <- newTChanIO
  ts <- newTVarIO (Map.fromList [(torrentID fooTorrent, (fooTorrent, tchan))])
  let torrentHandler = TorrentHandler ts undefined
  runServer torrentHandler 1100


