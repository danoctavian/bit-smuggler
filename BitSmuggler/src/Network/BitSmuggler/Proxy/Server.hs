module Network.BitSmuggler.Proxy.Server where

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
import Network.TCP.Proxy.Server as Proxy

import Network.BitSmuggler.Server as Server
import Network.BitSmuggler.Common
import Network.BitSmuggler.TorrentClientProc

import qualified Network.BitSmuggler.StreamMultiplex as Mux
import Network.BitSmuggler.Proxy.Common
import Network.BitSmuggler.Utils


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

proxyServer connData = Mux.runServer connData $ \conn -> do
  let src = newResumableSource $ connSource conn
  (postReqSrc, [ConnRequest (addr, port)]) <- src $$++ conduitGet get =$ DC.take 1

  (runTCPClient (clientSettings (fromIntegral port) (addrToBS addr)) $ \appData -> do
    DC.sourceList [DS.encode $ ConnSuccess $ toIP $ appSockAddr appData] $$ (connSink conn)
    void $ concurrently   
      (postReqSrc $$+- appSink appData) 
      (appSource appData $$ (connSink conn))
   -- in case connection fails
   ) `catchAny` (\_ -> DC.sourceList [DS.encode ConnFailure] $$ (connSink conn))
  return ()

addrToBS remote = BSC.pack $ case remote of
                                Left hostName -> hostName
                                Right ip -> show ip
