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
{-



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
listen :: ServerConfig -> IO ()
listen config = do
  debugM logger "started bit-smuggler server..."


rs :: [Word8]
rs = randoms (mkStdGen 2389353244732841002)


dumpRands = do
  putStrLn "dumping"
  runResourceT $ sourceLbs (BSL.pack rs) =$ DCB.isolate (10 ^ 6) $$ sinkFile "theRandFile"

