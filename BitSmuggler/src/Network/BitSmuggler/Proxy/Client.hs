module Network.BitSmuggler.Proxy.Client where


import System.Directory
import System.FilePath.Posix
import Data.Map as Map
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.Conduit.Cereal
import Data.Conduit.Network
import Data.Serialize as DS
import Control.Concurrent.Async
import Control.Exception
import Control.Monad

import Data.ByteString as BS
import Data.ByteString.Char8 as BSC


import Network.BitSmuggler.Client as Client 
import Network.BitSmuggler.Common
import Network.BitSmuggler.TorrentClientProc

import qualified Network.BitSmuggler.StreamMultiplex as Mux
import Network.BitSmuggler.Proxy.Common
import Network.BitSmuggler.Utils

descFileName = "serverDescriptor"

run :: IO ()  
run = do
  root <- getCurrentDirectory
  let (cachePath, btClientPath, contactsPath) = derivePaths root
  
  (ServerDescriptorFile ip serverPk contactsMeta) <- readConfigFile $ root </> descFileName 
  contacts <- readContactFiles contactsPath contactsMeta
  let serverDescriptor = ServerDescriptor ip contacts serverPk

  proc <- uTorrentProc btClientPath
  let btC = defaultBTConfig {
                 btProc = proc
               , outgoingRedirects = Map.empty -- no redirects
               }

  Client.clientConnect (ClientConfig btC serverDescriptor cachePath) proxyClient 
  return ()

 
proxyClient connData = do
  return ()

defaultBTConfig = BTClientConfig {
    pubBitTorrentPort = 5881
  , socksProxyPort = 2001
  , revProxyPort = 2002
  , cmdPort = 8000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = uTorrentConnect
}


