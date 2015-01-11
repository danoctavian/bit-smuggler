{-# LANGUAGE RecordWildCards, DeriveDataTypeable, OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Network.BitSmuggler.Common (
    ConnData (..)
  , BitSmugglerException (..)
  , BTClientConfig (..)
  , ContactFile (..)
  , ServerDescriptor (..)
  , setupContactFiles
  , createContactFile
  , setupBTClient
  , genRandBytes
) where

import Data.IP
import Data.ByteString
import Data.Torrent 
import Data.Word
import Data.Typeable
import Data.ByteString as BS
import Control.Retry
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Log.Logger
import System.FilePath
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import System.Random 
import Data.Conduit.Binary as DCB
import Data.Conduit
import System.Posix.Files
import Data.BEncode
import "temporary-resourcet" System.IO.Temp

import Network.BitSmuggler.Crypto (Key)
import Network.BitSmuggler.Utils
import Network.BitTorrent.ClientControl as CC hiding (Torrent) 
import Network.BitSmuggler.TorrentClientProc as TC
import Network.BitSmuggler.FileCache as FC
import Network.BitSmuggler.TorrentFile

{- 

bitsmuggler functionality common between client and server

-}

data BitSmugglerException = UnsupportedFeature
  deriving (Show, Typeable)

instance Exception BitSmugglerException 

logger = "BitSmuggler.common"

data ConnData = ConnData {
    connSend :: ByteString -> IO ()
  , connRecv :: IO ByteString
}

data BTClientConfig = BTClientConfig {
    pubBitTorrentPort :: PortNum
  , socksProxyPort :: PortNum
  , revProxyPort :: PortNum
  , cmdPort :: PortNum -- port on which it's receiving commands
  , btProc :: TorrentProc
    -- host, port, (uname, password)
  , connectToClient :: String -> Word16 -> IO TorrentClientConn
} 

{- a contact file is a file used to create a p2p file exchange
   between server and client.
   Such a file can either be randomly generated fake file
   OR a real file which can be found by its infohash
-}
data ContactFile = FakeFile {
                     seed :: Int -- the seed from which the file is created
                     -- a single file torrent; missing piece data
                   , torrentFile :: Torrent 
                   , infoHash :: InfoHash -- check whether this file is good
                   , useDHT :: Bool
                 }
                 | RealFile InfoHash


data ServerDescriptor = ServerDescriptor {
    serverAddr :: IP 
  , contactFiles :: [ContactFile]
  , serverPubKey :: Key
}

setupBTClient :: (MonadResource m) => BTClientConfig -> m (AsyncRes (), TorrentClientConn)
setupBTClient config = do
  let proc = btProc config
  liftIO $ cleanState proc
  liftIO $ TC.setSettings proc [CmdPort (cmdPort config)
                              , TC.BindPort (pubBitTorrentPort config)]

  btClient <- allocAsync (async $ start proc)
  liftIO $ threadDelay $ milli -- wait 1 second
  
  conn <- liftIO $ recovering (limitRetries 3 <> constantDelay milli) [alwaysRetry]
                 $ connectToClient config localhost (cmdPort config)

  {-
      Some of these settings are quirks of uTorrent so here the abstraction
      breaks. 
      UTP True - normally means it uses UTP but here it makes uTorrent use the proxies
                 and not circumvent them
      TODO: explain the rest of the settings
  -}
  liftIO $ CC.setSettings conn [UPnP False, NATPMP False, RandomizePort False, DHTForNewTorrents False, UTP True, LocalPeerDiscovery False, ProxySetType Socks4, ProxyIP localhost, ProxyPort (socksProxyPort config), ProxyP2P True]

  return (btClient, conn)


setupContactFiles contactFiles fileCachePath = do
  (_, cache) <- allocate (FC.load fileCachePath :: IO (FileCache InfoHash))
                 FC.close

  -- this dir will be deleted *recursively* when resource is cleared
  (_, contactsDir) <- createTempDirectory Nothing "contactFiles"

  forM contactFiles $ \f -> liftIO $ createContactFile f cache contactsDir


createContactFile contactFile cache dir = 
  case contactFile of
    f@(FakeFile {..})  -> do
      debugM logger "processing a fake file"
      let sf@(SingleFile {..}) = tInfo torrentFile -- assuming it's a single file
      let fname = (BSLC.unpack tName)
      let dataFile = dir </> fname
      createWithCache cache infoHash dataFile (genRandBytes seed (fromIntegral tLength))

      let torrentFilePath = dir </> fname
      when (not useDHT) $ do -- dump the torrent file
        tFileContent <- if (lacksPieces $ tInfo torrentFile) then do
          pieces <- hashPieces [dataFile] (fromIntegral tLength)
          return $ torrentFile {tInfo = sf {tPieces = BSL.fromChunks pieces}}
          else return torrentFile
        BSL.writeFile torrentFilePath (bPack $ serializeTorrent torrentFile) 

      return (dataFile, if useDHT then Left $ infoHash else Right torrentFilePath)

    RealFile {..} -> errorM logger "real files not supported"
                     >> throwIO UnsupportedFeature

createWithCache cache key filePath source = do
  maybeCached <- FC.lookup cache key
  target <- case maybeCached of 
    (Just cachedFile) -> return cachedFile
    Nothing -> put cache key source 
  createSymbolicLink target filePath

-- rand bytes file with an int seed
genRandBytes seed size
  = sourceLbs (BSL.pack  $ randoms (mkStdGen seed)) =$ DCB.isolate size

lacksPieces = (== "") . tPieces
