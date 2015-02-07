module Network.BitSmuggler.Proxy.Server where

import System.Directory
import System.FilePath.Posix
import Data.Map as Map
import qualified Data.ByteString.Lazy as BSL

import Network.BitSmuggler.Server as Server
import Network.BitSmuggler.Common
import Network.BitSmuggler.TorrentClientProc

import Network.BitSmuggler.Proxy.Common

{-
  proxy server for TCP traffic
  
-- doesn't support bind or UDP

-}

configFileName = "bitsmuggler.conf"

run :: IO ()
run = do
  root <- getCurrentDirectory
  let (cachePath, btClientPath, contactsPath) = derivePaths root

  (ServerConfigFile serverSk contactsMeta) <- readConfigFile $ root </> configFileName
  contacts <- readContactFiles contactsPath contactsMeta
  
  proc <- uTorrentProc btClientPath 
  let btC = defaultBTConfig {
                 btProc = proc
               , outgoingRedirects = Map.empty -- no redirects
               }

  Server.listen (ServerConfig serverSk btC contacts cachePath) $ proxyServer

  return ()

 -- TODO: make them configurable
defaultBTConfig = BTClientConfig {
    pubBitTorrentPort = 7881
  , socksProxyPort = 3001
  , revProxyPort = 3002
  , cmdPort = 9000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = uTorrentConnect
}

proxyServer = undefined

