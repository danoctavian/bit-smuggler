{-# LANGUAGE OverloadedStrings #-}
module ARQSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, pre, run)
import Data.Conduit as DC
import Data.Conduit.List as DC
import Prelude as P
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe as M
import Data.ByteString as BS

{-
import Control.Monad.ST
-}

import Network.BitSmuggler.Utils
import Network.BitSmuggler.ARQ

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "arq" $ do
    it "reliably delivers over a perfect channel" $ do -- easiest test
      quickCheckWith stdArgs { maxSuccess = 10 } $ reliablyDelivers slowStepPerfectMedium 
      return ()
    it "reliably delivers over a channel dropping packets sometimes" $ do -- easiest test
      quickCheckWith stdArgs { maxSuccess = 10 } $ reliablyDelivers (dropEveryNMedium 30)
      return ()
    return ()

  return ()

type StreamData = Maybe ByteString

reliablyDelivers medium leftData rightData 
  = monadicIO $ endToEndTest (toInputData leftData) (toInputData rightData) medium
    where
      toInputData = P.map (fmap ( BS.pack . (: [])) )

endToEndTest :: [StreamData] -> [StreamData] -> Conduit StreamData IO StreamData
                -> PropertyM IO ()
endToEndTest leftData rightData medium = do
  let packetSize = 1
  leftClock <- liftIO $ newClock 
  rightClock <- liftIO $ newClock
  leftARQ <- liftIO $ initGoBackNARQ  packetSize leftClock
  rightARQ <- liftIO $ initGoBackNARQ packetSize rightClock 

  leftRecvQueue <- liftIO $ newTQueueIO 
  rightRecvQueue <- liftIO $ newTQueueIO 

  leftToRight <- liftIO $ async $
    endToEndConduit (leftData, leftClock, leftARQ) (rightClock, rightARQ)
        medium (sinkTQueue rightRecvQueue)
  rightToLeft <- liftIO $ async $
    endToEndConduit (rightData, rightClock, rightARQ) (leftClock, leftARQ)
        medium (sinkTQueue leftRecvQueue)

  let expectedRight = M.catMaybes leftData
  let expectedLeft = M.catMaybes rightData


  -- suck those messages out 
  (rightReceived, leftReceived) <- liftIO $ concurrently 
    (sourceTQueue rightRecvQueue $$ DC.take (P.length expectedRight))
    (sourceTQueue leftRecvQueue $$ DC.take (P.length expectedLeft))

  liftIO $ cancel leftToRight
  liftIO $ cancel rightToLeft


  assert (expectedRight == rightReceived)
  assert (expectedLeft == leftReceived) 


endToEndConduit (theData, clock1, arq1) (clock2, arq2) medium sink = 
  DC.sourceList theData =$ (sendARQ arq1) =$ tickPipe (tickSend clock1)
  =$ medium
  =$ tickPipe (tickRecv clock2) =$ DC.catMaybes =$ (recvARQ arq2) $$ sink
  
tickPipe tick = DC.mapM (\m -> tick >> return m)

perfectMedium  :: Monad m => Conduit a m a
perfectMedium = DC.map P.id

slowStepDelay = threadDelay $ (10 ^ 2)

slowStepPerfectMedium = DC.mapM $ \m -> slowStepDelay >> return m

dropEveryNMedium n = go 3
  where
    go i = do
      upstream <- await
      liftIO $ slowStepDelay
      case upstream of
        Just m -> do
          if (i `mod` n /= 0) then DC.yield m 
            else return ()
          go (i + 1)
        Nothing -> return ()


