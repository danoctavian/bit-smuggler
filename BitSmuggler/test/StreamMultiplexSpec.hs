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
import Control.Monad.IO.Class
import Data.Maybe as M
import Data.ByteString as BS
import Control.Monad

{-
import Control.Monad.ST
-}

import Network.BitSmuggler.Utils
import Network.BitSmuggler.StreamMultiplex

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
    return ()
  return ()


streamsBothWays arbData1 arbData2
  = monadicIO $ testStream (toInputData arbData1) (toInputData arbData2)
  where
    toInputData = P.map BS.pack

testStream :: [ByteString] -> [ByteString] -> PropertyM IO ()
testStream clientToServer serverToClient = do
  -- setting up 2 way channel
  toServer <- liftIO $ newTQueueIO
  toClient <- liftIO $ newTQueueIO

  let clientConnData = ConnData {connSource = toProducer $ sourceTQueue toClient
                                , connSink = sinkTQueue toServer}
  let serverConnData = ConnData {connSource = toProducer $ sourceTQueue toServer 
                                , connSink = sinkTQueue toClient}


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
 

streamAndValidate connData recvData sendData
  = fmap fst $ concurrently
    ((connSource connData $$ DC.consume)
     >>= (\out -> return $ BS.concat out == BS.concat recvData))
    (DC.sourceList sendData $$ (connSink connData))
 

