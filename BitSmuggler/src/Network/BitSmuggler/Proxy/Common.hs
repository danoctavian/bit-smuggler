{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Network.BitSmuggler.Proxy.Common where

import System.FilePath.Posix
import Data.IP
import Data.Maybe
import Data.Aeson as A
import Data.Text as T
import Data.Text.Encoding
import Control.Applicative
import Control.Monad
import Data.Serialize as DS
import Data.ByteString.Base64 as Base64
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Torrent

import Network.BitSmuggler.Crypto
import Network.BitSmuggler.Common
import Network.BitSmuggler.Utils
import Network.BitSmuggler.TorrentFile

import Network.TCP.Proxy.Server (RemoteAddr)
import qualified Network.BitTorrent.ClientControl.UTorrent as UT


-- PROXY PROTOCOL
data ConnRequest = ConnRequest RemoteAddr
  deriving (Show, Eq)

instance Serialize IP where
  put (IPv4 ip) = putWord8 0 *> put ip
  put (IPv6 ip) = putWord8 1 *> put ip
  get = (byte 0 *> (IPv4 <$> get)) <|> (byte 1 *> (IPv6 <$> get))

-- maybe discard this. it's just a boolean
data ConnResponse = ConnSuccess (IP, PortNum) | ConnFailure
  deriving (Show, Eq)

instance Serialize ConnRequest where
  put (ConnRequest remote) = put remote
  get = ConnRequest <$> get 

instance Serialize ConnResponse where
  put (ConnSuccess remote) = putWord8 0 >> put remote
  put ConnFailure = putWord8 1
  get = (byte 0 >> (ConnSuccess <$> get)) <|> (byte 1 >> (return ConnFailure))

uTorrentConnect host port = UT.makeUTorrentConn host port ("admin", "")

derivePaths root = (root </> "cache", root </> "utorrent-client", root </> "contacts")

data ServerDescriptorFile = ServerDescriptorFile IP Key [ContactFileMetadata]
  deriving (Show, Eq)


data ServerConfigFile = ServerConfigFile Key [ContactFileMetadata]
  deriving (Show, Eq)
-- only supporting fake files at the moment
type ContactFileMetadata = (Int, FilePath, InfoHash)

-- parse the config files written with json
readConfigFile f = fmap (fromJust . A.decode) $ BSL.readFile f

readContactFiles contactsPath metadatas = do
  forM metadatas $ \(sd, filePath, ih) -> do
     t <- fmap (fromRight . readTorrent) $ BSL.readFile $ contactsPath </> filePath 
     return (FakeFile sd t ih)

instance FromJSON ServerDescriptorFile where
  parseJSON (Object o) = ServerDescriptorFile <$>
                         o .: "ip" <*> o .: "key" <*> o .: "contacts"
  parseJSON _ = mzero

instance FromJSON ServerConfigFile where
  parseJSON (Object o) = ServerConfigFile <$> o .: "key" <*> o .: "contacts"
  parseJSON _ = mzero

instance FromJSON ContactFileMetadata where
  parseJSON (Object o) = (,,) <$>  o .: "seed" <*> o.: "torrentFile" <*> o .: "hash"
  parseJSON _ = mzero

-- representing the server key 
textToKey :: Text -> Either String Key
textToKey t = Base64.decode (encodeUtf8 t) >>= DS.decode

keyToText :: Key -> Text
keyToText =  decodeUtf8 . Base64.encode . DS.encode 

instance FromJSON Key where
  parseJSON (String s) = case textToKey s of
    Left _ -> mzero
    Right k -> return k
  parseJSON _ = mzero

instance ToJSON Key where
  toJSON = String . keyToText

instance FromJSON InfoHash where
  parseJSON (String s) = case textToInfoHash s of
    Just ih -> return ih
    Nothing -> mzero

instance FromJSON IP where
  parseJSON (String s) = case maybeRead $ T.unpack s of
    Just ip -> return ip
    Nothing -> mzero
  parseJSON _ = mzero

