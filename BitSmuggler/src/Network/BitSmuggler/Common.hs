{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
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
  , findPieceLoader
  , ProxyDir (..)
  , startProxies
) where

import Prelude as P
import Data.IP
import Data.ByteString
import Data.Torrent 
import Data.Word
import Data.ByteString as BS
import qualified Data.Serialize as DS
import Control.Retry
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
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

import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature)
import Network.TCP.Proxy.Socks4 as Socks4

{- 

bitsmuggler functionality common between client and server

-}

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

      -- prepare the torrent file
      let torrentFilePath = dir </> fname
      tFileContent <- if (lacksPieces $ tInfo torrentFile) then do
        pieces <- hashPieces [dataFile] (fromIntegral tLength)
        return $ torrentFile {tInfo = sf {tPieces = BSL.fromChunks pieces}}
        else return torrentFile
      let computedIH = fromRight $ DS.decode $ computeInfoHash tFileContent
      when (computedIH /= infoHash) $ throwIO TorrentFileIntegrityFail
      BSL.writeFile torrentFilePath (bPack $ serializeTorrent tFileContent) 

      return (infoHash, (dataFile, torrentFilePath))

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

-- file fixing 
findPieceLoader contactFiles maybeIH = runMaybeT $ do
  ih <- hoistMaybe maybeIH
  (dataFilePath, tFilePath) <- hoistMaybe $ P.lookup ih contactFiles
  t <- fmap readTorrent $ liftIO $ BSL.readFile tFilePath 
  torrentFile <- hoistMaybe $ eitherToMaybe t
  liftIO $ makeBlockLoader (tInfo torrentFile) dataFilePath


-- == PROXYING ==

data ProxyDir = Forward | Reverse deriving (Show, Eq)

startProxies btConf onConn = do
  reverseProxy <- allocAsync $ async $ Proxy.run $ Proxy.Config {
                 proxyPort = revProxyPort btConf
               , initHook = P.flip (onConn Reverse)
               , handshake = revProxy (read localhost :: IP) (pubBitTorrentPort btConf)
             }

  -- forward socks 
  forwardProxy <- allocAsync $ async $ Proxy.run $ Proxy.Config {
                 proxyPort = socksProxyPort btConf
               , initHook = onConn Forward
               , handshake = Socks4.serverProtocol
            }
  return (reverseProxy, forwardProxy)

-- protocol for a rev proxy
-- just redirects all connections to a single address
revProxy ip port = return $ ProxyAction {
                            command = CONNECT
                          , remoteAddr = (Right ip, port)
                          , onConnection = \ _ -> return () 
                          }

