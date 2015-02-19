{-# LANGUAGE OverloadedStrings #-}
module StreamMultiplexSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, pre, run)
import Data.Conduit as DC
import Data.Conduit.List as DC
import Prelude as P
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent
import Control.Exception hiding (assert)

import Control.Monad.IO.Class
import Data.Maybe as M
import Data.ByteString as BS
import Data.Text as T
import System.Log.Logger

import Data.ByteString.Char8 as BSC

import Control.Monad
import Control.Monad.Trans.Resource

import Network.Curl
import Network.Curl.Opts
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp

{-
import Control.Monad.ST
-}

import Filesystem.Path.CurrentOS

import Network.BitSmuggler.Utils
import Network.BitSmuggler.StreamMultiplex

import Network.BitSmuggler.Proxy.Client as Proxy
import Network.BitSmuggler.Proxy.Server as Proxy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "mutiplex" $ do
    it "mutiplexes 1 connection" $ do -- easiest test
      P.putStrLn "todo"
      quickCheckWith stdArgs { maxSuccess = 50 } $ streamsBothWays
      return ()
    it "mutiplexes many connections" $ do 
      P.putStrLn "todo"
      return ()

  describe "mux tcp proxy" $ do
    it "proxies http requests for static files" $ P.putStrLn "wtf"
         >> (testHTTPProxy `catchAny`  (\e -> debugM logger $ "EXCEPTION :" ++ show e))
  return ()

testHTTPProxy = void $ forM [1..10] $ \i -> runResourceT $  do
--  liftIO $ updateGlobalLogger logger  (setLevel DEBUG)

  let root = "test-data/test-server-root"
  let serverPort = 3333
  let proxyPort = 1080
  let app = staticApp $ defaultWebAppSettings (fromText root)

  allocAsync $ async $ Warp.run serverPort app

  (clientConnData, serverConnData) <- liftIO $  initSTMConnData

  allocLinkedAsync $ async
    $ Proxy.proxyServer serverConnData  `catchAny` (\e -> do
      debugM logger $ "terminated the server thread " ++ show e
      throwIO e)
  allocLinkedAsync $ async
    $ (Proxy.proxyClient proxyPort clientConnData) `catchAny` (\e -> do
        debugM logger $ "terminated the client thread " ++ show e
        throwIO e)

  liftIO $ waitForServer (BSC.pack localhost) (fromIntegral serverPort)
  liftIO $ waitForServer (BSC.pack localhost) (fromIntegral proxyPort)

  liftIO $ debugM logger "the servers are available. continuing with testing ..."

  -- run concurrent requests
  results <- liftIO $ (P.flip mapConcurrently)
           (P.take 10 $ P.cycle ["tinyFile.txt", "tinyFile0.txt", "tinyFile1.txt"])
    $ \fileName -> do
      let fullPath = (fromText root) </> (fromText fileName)
      contents <- liftIO $ P.readFile (T.unpack $ fromRight $ toText fullPath)
      (code, proxiedContents) <- liftIO $ curlGetString
         (localhost ++ ":" ++ (show serverPort) ++ "/" ++ T.unpack fileName)
         [Network.Curl.Opts.CurlProxy $ "socks4://127.0.0.1:" ++ (show proxyPort)]
      debugM logger "evaluate the results"
      code `shouldBe` CurlOK
      proxiedContents `shouldBe` contents

  liftIO $ debugM logger "DONE RUNNING TEST"
  return ()

streamsBothWays arbData1 arbData2
  = monadicIO $ testStream (toInputData arbData1) (toInputData arbData2)
  where
    toInputData = P.map BS.pack

testStream :: [ByteString] -> [ByteString] -> PropertyM IO ()
testStream clientToServer serverToClient = do
  -- setting up 2 way channel
  (clientConnData, serverConnData) <- liftIO $ initSTMConnData

  clientResult <- liftIO $ newEmptyTMVarIO 
  serverResult <- liftIO $ newEmptyTMVarIO

  tid <- liftIO $ forkIO $ void $ concurrently 
    (runClient clientConnData
      $ (\initConn -> initConn
        (\connData -> streamAndValidate connData serverToClient clientToServer
                      >>= (\r -> atomically $ putTMVar clientResult r) )))
    (runServer serverConnData
      (\connData -> streamAndValidate connData clientToServer serverToClient
                    >>= (\r -> atomically $ putTMVar serverResult r) ))

  clientSendsPayload <- liftIO $ atomically $ takeTMVar clientResult
  serverSendsPayload <- liftIO $ atomically $ takeTMVar serverResult

  assert clientSendsPayload
  assert serverSendsPayload

  liftIO $ killThread tid
      
  return ()

initSTMConnData = do
  toServer <- newTQueueIO
  toClient <- newTQueueIO

  let clientConnData = ConnData {connSource = toProducer $ sourceTQueue toClient
                                , connSink = sinkTQueue toServer}
  let serverConnData = ConnData {connSource = toProducer $ sourceTQueue toServer 
                                , connSink = sinkTQueue toClient}
  return (clientConnData, serverConnData)

streamAndValidate connData recvData sendData
  = fmap fst $ concurrently
    ((connSource connData $$ DC.consume)
     >>= (\out -> return $ BS.concat out == BS.concat recvData))
    (DC.sourceList sendData $$ (connSink connData))
