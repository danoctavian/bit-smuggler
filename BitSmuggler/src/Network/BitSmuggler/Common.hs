{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Network.BitSmuggler.Common (
    ConnData (..)
  , BitSmugglerException (..)
  , BTClientConfig (..)
  , ContactFile (..)
  , ServerDescriptor (..)
  , setupContactFiles
  , setupContactFilesLazy
  , createContactFile
  , setupBTClient
  , genRandBytes
  , findPieceLoader
  , ProxyDir (..)
  , startProxies
  , addTorrents
  , makeRandPartial
  , giveClientPartialFile
  , isUsedUp
  , expectedMaxProgress
  , replenishFile
) where

import Prelude as P
import Data.Maybe
import Data.IP
import Data.ByteString
import Data.Torrent as TF
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
import System.IO hiding (openTempFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import System.Random 
import Data.Conduit.Binary as DCB
import Data.Conduit
import System.Posix.Files
import Data.BEncode
import "temporary-resourcet" System.IO.Temp
import qualified Data.Set as Set
import Data.Map as Map

import Data.Random.RVar
import Data.Random.Extras
import Data.Random.Source.DevRandom

import System.IO.Unsafe

import Network.BitSmuggler.Crypto (Key)
import Network.BitSmuggler.Utils
import Network.BitTorrent.ClientControl as CC
import Network.BitSmuggler.TorrentClientProc as TC
import Network.BitSmuggler.FileCache as FC
import Network.BitSmuggler.TorrentFile

import Network.TCP.Proxy.Server as Proxy hiding (UnsupportedFeature, logger)
import Network.TCP.Proxy.Socks4 as Socks4

{- 

bitsmuggler functionality common between client and server

-}

data BTClientConfig = BTClientConfig {
    pubBitTorrentPort :: PortNum
  , socksProxyPort :: PortNum
  , revProxyPort :: PortNum
  , cmdPort :: PortNum -- port on which it's receiving commands
  , btProc :: TorrentProc
    -- host, port, (uname, password)
  , connectToClient :: String -> Word16 -> IO TorrentClientConn

  , outgoingRedirects :: Map RemoteAddr RemoteAddr
} 

{- a contact file is a file used to create a p2p file exchange
   between server and client.
   Such a file can either be randomly generated fake file
   OR a real file which can be found by its infohash
-}
data ContactFile = FakeFile {
                     seed :: Int -- the seed from which the file is created
                     -- a single file torrent; missing piece data
                   , torrentFile :: TF.Torrent 
                   , infoHash :: InfoHash -- check whether this file is good
                 }
                 | RealFile {infoHash :: InfoHash}


data ServerDescriptor = ServerDescriptor {
    serverAddr :: IP 
  , contactFiles :: [ContactFile]
  , serverPubKey :: Key
}

setupBTClient :: (MonadResource m) => BTClientConfig
              -> m ((TorrentProc, AsyncRes ()), TorrentClientConn)
setupBTClient config = do
  let proc = btProc config
  liftIO $ cleanState proc
  liftIO $ TC.setSettings proc [CmdPort (cmdPort config)
                              , TC.BindPort (pubBitTorrentPort config)]

  liftIO $ debugM logger "started bittorrent client proc"

  btClient <- allocLinkedAsync (async $ start proc)
  liftIO $ threadDelay $ milli -- wait 1 second
  
  conn <- liftIO $ recovering (limitRetries 4 <> constantDelay milli) [alwaysRetry]
                 $ do
                  debugM logger "attempting to connect to bittorrent client..."
                  connectToClient config localhost (cmdPort config)

  liftIO $ CC.setSettings conn [CC.BindPort (pubBitTorrentPort config)
          , UPnP False, NATPMP False, RandomizePort False, DHTForNewTorrents False
          , TransportDisposition True False True False
          , LocalPeerDiscovery False
          , LimitLocalPeerBandwidth True
          , UploadSlotsPerTorrent 1
          , ProxySetType Socks4, ProxyIP localhost, ProxyPort (socksProxyPort config)
          , ProxyP2P True]

  return ((proc, btClient), conn)


setupContactFiles contactFiles fileCachePath = setupContacts contactFiles fileCachePath id 

-- this performs lazy IO
setupContactFilesLazy contactFiles fileCachePath
  = setupContacts contactFiles fileCachePath (return . unsafePerformIO)

setupContacts  contactFiles fileCachePath execute = do
  (_, cache) <- allocate (FC.load fileCachePath :: IO (FileCache InfoHash))
                 FC.close
  -- this dir will be deleted *recursively* when resource is cleared
  (_, contactsDir) <- createTempDirectory Nothing "contactFiles"

  forM contactFiles $ \f -> fmap (infoHash f,) $
                          liftIO $ execute $ createContactFile f cache contactsDir


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

{-
      TODO: fix computeInfoHash and bring back this section of code
      let computedIH = fromRight $ DS.decode $ computeInfoHash tFileContent
      when (computedIH /= infoHash) $ throwIO TorrentFileIntegrityFail
-}

      return (dataFile, tFileContent)

    RealFile {..} -> errorM logger "real files not supported"
                     >> throwIO UnsupportedFeature

createWithCache cache key filePath source = do
  maybeCached <- FC.lookup cache key
  target <- case maybeCached of 
    (Just cachedFile) -> do
      debugM logger "file is already in the cache"
      return cachedFile
    Nothing -> put cache key source 
--  debugM logger $ "creating symbolic link between " ++ target ++ " and " ++ filePath
  createSymbolicLink target filePath

-- rand bytes file with an int seed
genRandBytes seed size
  = sourceLbs (BSL.pack  $ randoms (mkStdGen seed)) =$ DCB.isolate size

lacksPieces = (== "") . tPieces

-- file fixing 
findPieceLoader contactFiles ih = runMaybeT $ do
  (dataFilePath, torrentFile) <- hoistMaybe $ P.lookup ih contactFiles
  liftIO $ makeBlockLoader (tInfo torrentFile) dataFilePath


-- == FILE USAGE ==
-- telling the bittorrent client to use a bunch of torrent files

addTorrents btClientConn btProc files = do
  -- construct partials
  -- this can be parallelized
  forM files $ \file@(infoHash, (dataFile, torrentFile)) -> do
    debugM logger $ "adding for torrenting the file " P.++ (show dataFile)
    giveClientPartialFile btClientConn btProc file
    liftIO $ debugM logger $ "finished adding file" P.++ (show dataFile)
  return ()

giveClientPartialFile btClientConn btProc (infoHash, (dataFile, torrentFile)) = do
  let sf@(SingleFile {..}) = tInfo torrentFile -- assuming it's a single file
  let pieceCount = tLength `div` tPieceLength 
  let fName = (BSLC.unpack tName)
  let btPath = getFilePath btProc fName

  -- create partial file in bittorrent client's file store
  makeRandPartial (fromIntegral tPieceLength) (fromIntegral pieceCount)
                   dataFile  btPath

  debugM logger $ "created a partially completed file" P.++ (show btPath)

  runResourceT $ do
    (_, path, handle) <- openTempFile Nothing (fName ++ ".torrent")
    liftIO $ BS.hPutStr  handle (BSL.toStrict $ bPack $ serializeTorrent torrentFile)
    liftIO $ hClose handle


    liftIO $ debugM logger $ "telling  the bt client to use " P.++ (show dataFile)
    liftIO $ debugM logger $ "with torrent file " P.++ (show path)

    liftIO $ addTorrentFile btClientConn path

    -- wait for it to upload
    retrying (constantDelay $ 10 ^ 5 * 5) (\_ isUploaded -> return $ not isUploaded)
      $ do
        torrentList <- (liftIO $ listTorrents btClientConn)
        return $ (P.elem infoHash) . P.map torrentID $ torrentList

    -- cap the transfer rate
    -- TODO: play with this some more - right now even though you set
    -- it to 300kb it gradually grows from 0 to 30-40 very slowly 
    {-
    let maxTransferRate = 300000 -- make this configurable or adaptable
    liftIO $ setJobProperties btClientConn infoHash 
                [DownloadRate maxTransferRate, UploadRate maxTransferRate]
    -}
  return ()

-- pieceCount / pieceCountDiv are how many pieces of a file are 
-- owned by the local client
-- the value x = 1 / 2 maximises the function f(x) = 1 - x ^ 2 
-- where f determines roughly how many pieces are exchanged both ways
-- if both clients start out with pieceCount / pieceCountDiv random pieces
pieceCountDiv = 2

maxProgress :: Int
maxProgress = 1000

-- a rough estimate of what progress each peer makes if they 
-- were to randomly own a certain fraction of the file and then exchange
expectedMaxProgress
  = maxProgress `div` pieceCountDiv
    + (round $ (1 - fraction) * fraction * (fromIntegral maxProgress))
  where
    fraction = 1 / (fromIntegral pieceCountDiv)

-- a torrent is used up if it downloaded enough pieces that it 
-- is not able to build lengthy connections anymore for both
-- downstream and upstream (the downstream is the problem here)
isUsedUp (CC.Torrent {..})
  = (progress >= expectedMaxProgress - expectedMaxProgress `div` 10)
    && ((peersConnected == 0 && seedsConnected == 0) || (downSpeed < 10 ^ 4)) 

-- make a file with only part of the pieces present, chosen at random
makeRandPartial pieceSize pieceCount origin dest = do
  chosenIxs <- runRVar (sample (pieceCount `div` pieceCountDiv)
                       [0..(pieceCount - 1)]) DevURandom
  let pieceSet = Set.fromList chosenIxs
  makePartial pieceSize origin dest (\i _ -> Set.member i pieceSet)
 
-- == PROXYING ==

data ProxyDir = Forward | Reverse deriving (Show, Eq)

startProxies btConf onConn = do
  reverseProxy <- allocLinkedAsync $ async $ Proxy.run $ Proxy.Config {
                 proxyPort = revProxyPort btConf
               , initHook = P.flip (onConn Reverse)
               , handshake = revProxy (read localhost :: IP) (pubBitTorrentPort btConf)
               , makeConn = Proxy.directTCPConn
             }

  -- forward socks 
  forwardProxy <- allocLinkedAsync $ async $ Proxy.run $ Proxy.Config {
                 proxyPort = socksProxyPort btConf
               , initHook = onConn Forward
               , handshake = Socks4.serverProtocol

                -- applying any potential redirects to outgoing connections
               , makeConn = \remote handle ->
                       directTCPConn (redirect remote $ outgoingRedirects btConf) handle
            }
  return (reverseProxy, forwardProxy)

redirect addr redirectMap = maybe addr id (Map.lookup addr redirectMap)


-- protocol for a rev proxy
-- just redirects all connections to a single address
revProxy ip port = return $ ProxyAction {
                            command = CONNECT
                          , remoteAddr = (Right ip, port)
                          , onConnection = \ _ -> return () 
                          }


-- == FILE REPLENISHING ==

replenishFile btClientConn btProc files infoHash = do
  let diskFilePaths = fromJust $ P.lookup infoHash files
  -- pull out the file 
  removeTorrentWithData btClientConn infoHash

  giveClientPartialFile btClientConn btProc (infoHash, diskFilePaths)



