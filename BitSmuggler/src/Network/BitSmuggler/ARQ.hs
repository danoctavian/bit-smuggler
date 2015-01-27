{-# LANGUAGE RecordWildCards #-}

module Network.BitSmuggler.ARQ where

import Prelude as P
import Foreign.Storable
import Data.Word
import Data.Default
import Data.ByteString as BS
import Data.Dequeue
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe

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
                | AckMessage {ackNum :: PacketNum}
    deriving (Show, Eq)

-- regardless of content, size is preserved
instance Serialize ARQMessage where
  put (AckMessage ack) = putWord8 0 >> putWord32be 0 >> putWord32be ack
  put (DataMessage seq ack payload) = putWord8 1 >> putWord32be seq >> putWord32be ack

  get = (byte 0 *> (AckMessage <$> (getWord32be *> getWord32be)))

-- time is measured in ticks
-- a tick being a bittorrent piece passing through the stream
data Time = Time {sendTime :: Int, recvTime :: Int} deriving (Show, Eq)

initGoBackNARQ clock = do
  stateVar <- newTVarIO $ ARQState {
                     needsAck = False
                   , currentAck = 0
                   , buffer = []
                   , currentSeq = 1
                   , maxBuffer = 10 -- debatable
                }
  return $ ARQ {
      sendARQ = sendHook stateVar clock
    , recvARQ = awaitForever $ \raw -> do
        let msg = fromRight $ DS.decode raw
        maybePacket <- liftIO $ atomically $ do
          state <- readTVar stateVar
          -- treat the ack
          modifyTVar stateVar (\s -> s {buffer = 
                                 P.filter ( (<= ackNum msg) . msgSeq) $ buffer state})

          -- treat the data 
          case msg of 
            (AckMessage ack) -> return Nothing
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

sendHook :: TVar ARQState -> TVar Time -> ARQPipe (Maybe ByteString)
sendHook stateVar clock = do
  (arqMsg, lastAck) <- liftIO $ atomically $ do
    now <- readTVar clock  
    state <- readTVar stateVar
    let (afterResend, resendable) = findAndModify (buffer state)
                              (isOld now . timestamp) (\m -> m {timestamp = now})
    modifyTVar stateVar (\s -> s {buffer = afterResend, needsAck = False})
    let lastAck = currentAck state
    return $ (mplus (fmap (bufferedToMessage lastAck) resendable)
                  (if needsAck state then Just $ AckMessage lastAck else Nothing), lastAck)
  case arqMsg of
    (Just dm@(DataMessage {..})) -> (DC.yield $ Just $ DS.encode dm)
                                     >> sendHook stateVar clock
    other -> do
      stateNow <- liftIO $ atomically $ readTVar stateVar
      upstream <- await
      case upstream of
        Nothing -> do -- upstream termination
          if (buffer stateNow == [] && isNothing other) then return () -- terminate send arq
          else DC.yield (fmap DS.encode $ other) >> sendHook stateVar clock
        -- no user message available now; send any potential ARQ and loop
        (Just Nothing) -> DC.yield (fmap DS.encode other) >> sendHook stateVar clock 
        -- new user message
        (Just (Just msg)) -> do
          now <- liftIO $ atomically $ readTVar clock
          let bufMsg = BufferedMsg now (currentSeq stateNow) msg
          liftIO $ atomically $ modifyTVar stateVar
                                (\s -> s { currentSeq = currentSeq stateNow + 1
                                         , buffer =  bufMsg : (buffer stateNow)})

          DC.yield $ (Just $ DS.encode $
                      DataMessage (currentSeq stateNow) (currentAck stateNow) msg)
          sendHook stateVar clock
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


