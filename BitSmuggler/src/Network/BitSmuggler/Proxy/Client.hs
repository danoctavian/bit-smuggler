module Network.BitSmuggler.Proxy.Client (
    proxyClient
  , Network.BitSmuggler.Proxy.Client.run  
) where

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

import System.Log.Logger

import Data.ByteString as BS
import Data.ByteString.Char8 as BSC

import Network.TCP.Proxy.Server as Proxy hiding (logger)
import Network.TCP.Proxy.Socks4 as Socks4


import Network.BitSmuggler.Client as Client 
import Network.BitSmuggler.Common
import Network.BitSmuggler.TorrentClientProc

import qualified Network.BitSmuggler.StreamMultiplex as Mux
import Network.BitSmuggler.Proxy.Common
import Network.BitSmuggler.Utils

descFileName = "server-descriptor"

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

  Client.clientConnect (ClientConfig btC serverDescriptor cachePath)
                       $ proxyClient defaultSocksProxyPort
  return ()

defaultSocksProxyPort = 9999

defaultBTConfig = BTClientConfig {
    pubBitTorrentPort = 5881
  , socksProxyPort = 2001
  , revProxyPort = 2002
  , cmdPort = 8000 -- port on which it's receiving commands
    -- host, port, (uname, password)
  , connectToClient = uTorrentConnect
}

proxyClient socksPort connData = do
  Mux.runClient connData $ \initMuxConn ->  do
    Proxy.run $ Config {
        proxyPort = socksPort  
      , initHook = Proxy.initNoOpHook 
      , handshake = Socks4.serverProtocol
      , makeConn = \remote proxyData -> do 
         initMuxConn $ \conn -> do
           DC.sourceList [DS.encode $ ConnRequest remote] $$ (connSink conn) 
           (postResSrc, [response]) <- (connSource conn) $$+ conduitGet get =$ DC.take 1 
           case response of
             ConnFailure -> throwIO Proxy.ConnectionFailed 
             ConnSuccess ip -> do
               -- TODO: i'm not sure about this working correctly
               (newSrc, finalize) <- unwrapResumable postResSrc
               proxyData (toProducer newSrc) (connSink conn) ip `finally` (finalize)
      }
