module Network.BitSmuggler.Protocol where

import Data.IP
import Network.BitSmuggler.Crypto (Key)
import Network.BitTorrent.ClientControl hiding (Torrent)
import Data.ByteString
import Data.Torrent 

{- 

handling bitsmuggler data streams

-}


data ConnData = ConnData {
    connSend :: ByteString -> IO ()
  , connRecv :: IO ByteString
}

data BTClientConfig = BTClientConfig {
    pubBitTorrentPort :: PortNum
  , socksProxyPort :: PortNum
  , revProxyPort :: PortNum
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
                   , fileInfoHash :: InfoHash 
                 }
                 | RealFile InfoHash


data ServerDescriptor = ServerDescriptor {
    serverAddr :: IP 
  , contactFiles :: [ContactFile]
  , serverPubKey :: Key
}
