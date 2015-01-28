{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.BitSmuggler.ARQ where

import Prelude as P
import Foreign.Storable
import Data.Word
import Data.Default
import Data.ByteString as BS
import Data.Dequeue
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent.Async
import Data.Maybe as M
import Control.Exception.Base
import System.Log.Logger

import Data.Serialize as DS
import Data.Conduit as DC
import Data.Conduit.List as DC

import Network.BitSmuggler.Utils

{-
 == Automatic repeat request ARQ protocol ==
  
  protocol to deal with
    * disconnects in the bittorrent client connection
    * tcp packet tampering by an adversary

  type: go-back-n ARQ

  implementation is specific to the bitsmuggler design

  TODO: it's more imperative and impure than it should be. refactor later
-}


type PacketNum = Word32

headerLen = sizeOf (def :: Word8) + sizeOf (def :: PacketNum) + sizeOf (def :: PacketNum)


data ARQ = ARQ {
    sendARQ :: ARQPipe (Maybe ByteString)
  , recvARQ :: ARQPipe ByteString
}

type ARQPipe a = Conduit a IO a

noARQ :: ARQ 
noARQ = ARQ (DC.map id) (DC.map id)

data ARQState = ARQState {
    needsAck :: Bool
  , currentAck :: PacketNum
  , buffer :: [BufferedMsg]
  , currentSeq :: PacketNum
  , maxBuffer :: Int
}

data BufferedMsg = BufferedMsg {
                    timestamp :: Time
                  , msgSeq :: PacketNum
                  , payload :: ByteString
                } deriving (Show, Eq)

data ARQMessage = DataMessage { seqNum :: PacketNum
                              , ackNum :: PacketNum
                              , arqPayload :: ByteString
                          }
                | AckMessage {ackNum :: PacketNum, paddingLen :: Int}
    deriving (Show, Eq)

-- regardless of content, header size is preserved
instance Serialize ARQMessage where
  -- apply some random padding - doesn't matter
  put (AckMessage ack padLen) = putWord8 0 >> putWord32be 0 >> putWord32be ack
                              >> putByteString (BS.replicate padLen 11)
  put (DataMessage seq ack payload) = putWord8 1 >> putWord32be seq >> putWord32be ack
                                    >> putByteString payload

  get = (byte 0 *> (AckMessage <$> (getWord32be *> getWord32be) <*>
                                    (BS.length <$> getRemaining)))
        <|>
        (byte 1 *> (DataMessage <$> getWord32be <*> getWord32be <*> getRemaining))

-- time is measured in ticks
-- a tick being a bittorrent piece passing through the stream
data Time = Time {sendTime :: Int, recvTime :: Int} deriving (Show, Eq)

newClock = newTVarIO $ Time 0 0 
tickSend timeVar = atomically $ modifyTVar timeVar (\t -> t {sendTime = sendTime t + 1})
tickRecv timeVar = atomically $ modifyTVar timeVar (\t -> t {recvTime = recvTime t + 1})


debugP name s = debugM logger $ name P.++ s

initGoBackNARQ clock packetSize name = do
  stateVar <- newTVarIO $ ARQState {
                     needsAck = False
                   , currentAck = 0
                   , buffer = []
                   , currentSeq = 1
                   , maxBuffer = 10 -- debatable
                }
  return $ ARQ {
      sendARQ = sendHook stateVar clock packetSize
    , recvARQ = awaitForever $ \raw -> do
        let msg = fromRight $ DS.decode raw

--        liftIO $ debugP name $ "got ack " P.++ (show $ ackNum msg)
        maybePacket <- liftIO $ atomically $ do
          state <- readTVar stateVar
          -- treat the ack
          modifyTVar stateVar (\s -> s {buffer = 
                                 P.filter ( (> ackNum msg) . msgSeq) $ buffer s})

          -- treat the data 
          case msg of 
            (AckMessage ack _) -> return Nothing
            (DataMessage seq ack payload) ->
              if (seq == currentAck state + 1)
              then do
                modifyTVar stateVar (\s -> s {currentAck = seq, needsAck = True})
                return $ Just payload -- push the message content
              else return Nothing -- out of sequence
        case maybePacket of
          Just payload -> DC.yield payload
          Nothing -> return ()
  }

sendHook :: TVar ARQState -> TVar Time -> Int -> ARQPipe (Maybe ByteString)
sendHook stateVar clock packetSize = do

--  liftIO $ debugM logger "looping send hook"
  let loop = sendHook stateVar clock packetSize
  (arqMsg, lastAck) <- liftIO $ atomically $ do
    now <- readTVar clock  
    state <- readTVar stateVar
    let (afterResend, resendable) = findAndModify (buffer state)
                              (isOld now . timestamp) (\m -> m {timestamp = now})
    modifyTVar stateVar (\s -> s {buffer = afterResend, needsAck = False})
    let lastAck = currentAck state
    return $ (mplus (fmap (bufferedToMessage lastAck) resendable)
                  (if needsAck state then Just $ AckMessage lastAck (packetSize - headerLen)  else Nothing)
                  , lastAck)
  case arqMsg of
    (Just dm@(DataMessage {..})) -> (DC.yield $ Just $ DS.encode dm)
                                     >> sendHook stateVar clock packetSize
    other -> do
      stateNow <- liftIO $ atomically $ readTVar stateVar
--      liftIO $ debugM logger $ "buffer is " P.++ (show $ buffer stateNow)
      upstream <- await
      case upstream of
        Nothing -> do -- upstream termination
          -- there's no proper termination for ARQ just leave it running forever
--          if (buffer stateNow == [] && isNothing other) then return () 
          DC.yield (fmap DS.encode $ other) >> loop 
        -- no user message available now; send any potential ARQ and loop
        (Just Nothing) -> DC.yield (fmap DS.encode other) >> loop 
        -- new user message
        (Just (Just msg)) -> do
          now <- liftIO $ atomically $ readTVar clock
          let bufMsg = BufferedMsg now (currentSeq stateNow) msg
          liftIO $ atomically $ modifyTVar stateVar
                                (\s -> s { currentSeq = currentSeq s + 1
                                         , buffer =  bufMsg : (buffer s)})

          DC.yield $ (Just $ DS.encode $
                      DataMessage (currentSeq stateNow) (currentAck stateNow) msg)
          loop 
  return ()


bufferedToMessage lastAck bm = DataMessage { seqNum = msgSeq bm
                                            , arqPayload = payload bm
                                            , ackNum = lastAck}

-- the values are handcrafted at the moment
isOld now timestamp = (sendTime now - sendTime timestamp >= 2) 
                    && (recvTime now - recvTime timestamp >= 4) 

findAndModify (x : xs) pred f
  | pred x = ((f x) : xs, Just x)
  | otherwise = let (rest, item) = findAndModify xs pred f in (x : rest, item)
findAndModify [] pred f = ([], Nothing)


--- DEBUGGING

endToEndTest leftData rightData medium = do
  let packetSize = 1 + headerLen
  leftClock <- newClock 
  rightClock <- newClock
  leftARQ <- initGoBackNARQ  leftClock packetSize "left"
  rightARQ <- initGoBackNARQ rightClock packetSize "right"

  leftRecvQueue <- newTQueueIO 
  rightRecvQueue <- newTQueueIO 

  leftToRight <- async $
    endToEndConduit (leftData, leftClock, leftARQ) (rightClock, rightARQ)
        medium (sinkTQueue rightRecvQueue)
  rightToLeft <- async $
    endToEndConduit (rightData, rightClock, rightARQ) (leftClock, leftARQ)
        medium (sinkTQueue leftRecvQueue)

  let expectedRight = M.catMaybes rightData 
  let expectedLeft = M.catMaybes leftData

  -- suck those messages out 
  (rightReceived, leftReceived) <- concurrently 
    (sourceTQueue rightRecvQueue $$ DC.take (P.length expectedRight))
    (sourceTQueue leftRecvQueue $$ DC.take (P.length expectedLeft))


  cancel leftToRight
  cancel rightToLeft

  debugM logger $ "right received " P.++ (show rightReceived)
  debugM logger $ "left received " P.++ (show leftReceived)

  assert (expectedRight == rightReceived) (return ())
  assert (expectedLeft == leftReceived) (return ())


endToEndConduit (theData, clock1, arq1) (clock2, arq2) medium sink = 
  DC.sourceList theData =$ (sendARQ arq1) =$ tickPipe (tickSend clock1)
  =$ medium
  =$ tickPipe (tickRecv clock2) =$ DC.catMaybes =$ (recvARQ arq2) $$ sink
  
tickPipe tick = DC.mapM (\m -> tick >> return m)

perfectMedium  :: Monad m => Conduit a m a
perfectMedium = DC.map P.id

slowStepDelay = threadDelay $ (10 ^ 5 * 1)

slowStepPerfectMedium = DC.mapM $ \m -> slowStepDelay >> return m

dropEveryNMedium n = go 200
  where
    go i = do
      upstream <- await
      liftIO $ slowStepDelay
      case upstream of
        Just m -> do
          if (i `mod` n /= 0) then DC.yield m 
            else liftIO $ debugM logger $ "dropped packet " P.++ (show m)
          go (i + 1)
        Nothing -> return ()

runARQTest = do
  updateGlobalLogger logger  (setLevel DEBUG)
  let sampleData = [Just "1", Nothing, Just "2", Just "3", Nothing, Just "4", Nothing, Nothing, Just "5", Just "6", Just "7", Just "8", Just "9", Just "10", Just "11", Nothing, Nothing, Nothing, Nothing, Just "12", Just "13", Just "14", Nothing, Nothing, Just "15", Just "16", Just "17", Just "18", Nothing, Just "19", Nothing, Nothing, Nothing, Just "20", Just "21", Nothing, Just "22", Just "23", Just "24", Just "25", Nothing, Nothing, Just "26", Nothing, Just "28", Nothing, Nothing, Just "29", Nothing, Nothing, Just "30", Just "31", Just "32", Nothing, Nothing]
  let bigSample = sampleData --P.concat $ P.replicate 2 sampleData
  
  endToEndTest sampleData bigSample  (dropEveryNMedium 20) 
