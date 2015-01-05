module Network.BitSmuggler.Server where

import Prelude as P
import Network.BitSmuggler.Crypto (Key)
import System.Log.Logger
import Network.BitTorrent.ClientControl
import System.Random
import Data.Word
import Data.Conduit.Binary as DCB
import Data.Conduit
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString

import Network.BitSmuggler.Protocol
{-

SERVER.

run single torrent client - running many potentially blows the cover
 a normal peer in the bittorrent network runs a single instance of
 some torrent client

-}

logger = "BitSmuggler.Server"


{-

rev proxy
socks proxy
bt client private port
bt client cmd server port


-}


data ServerConfig = ServerConfig {
    serverSecretKey :: Key
  , pubBitTorrentPort :: PortNum
    
}

data ServerInternals = ServerInternals {
    socksProxyPort :: PortNum
  , revProxyPort :: PortNum
}


listen :: ServerConfig -> (ConnData -> IO ()) -> IO ()
listen config handle = do
  debugM logger "started bit-smuggler server..."


  -- start torrent client (with config)

  -- connect to it
  -- configure it

  -- setup the files on which the client is working


  -- setup proxies (socks and reverse)

  -- wait for it...


genRandFile :: Int -> Int -> FilePath -> IO ()
genRandFile seed size file = runResourceT
              $ sourceLbs (BSL.pack  $ randoms (mkStdGen seed))
              =$ DCB.isolate size $$ sinkFile file

