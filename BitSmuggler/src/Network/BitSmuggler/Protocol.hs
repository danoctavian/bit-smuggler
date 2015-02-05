{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.BitSmuggler.Protocol where

import Prelude as P hiding (read)
import Data.Maybe
import Data.Serialize as DS
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Conduit.Cereal
import Data.Conduit.Cereal.Internal

import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Control.Monad as CM
import Control.Monad.Trans
import Control.Applicative hiding (empty)
import Control.Exception
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Crypto.Random
import System.Log.Logger
import System.IO.Unsafe -- tODO: please for the love of god remove this
import Control.Exception.Base

import Network.BitSmuggler.Crypto as Crypto
import Network.BitSmuggler.Utils
import qualified Network.BitSmuggler.BitTorrentParser as BT

import Network.BitSmuggler.ARQ as ARQ
import Network.BitSmuggler.Common
import Network.BitSmuggler.TorrentFile


{-

handling data streams
-}

-- the size of a packet embedded in a bittorrent piece message
-- packetSize = blockSize - Crypto.msgHeaderLen - ARQ.headerLen

{- the unofficial standard size of a bittorrent block
   WARNING: This is a strong assumption
   according to the protocol the block size
   is variable depending on the specified size in the
   request message.
   At this point it greatly simplifies the bitSmuggler protocol
   and in nature this is what most bittorrent clients use


   Steg stream:
    the current design and above assumption allows to easily plug in
    some steganography with a *constant* expansion factor per packetSize
    
    Eg.: some clever way of taking a piece of cyphertext and making it
    match the distribution properties of a video codec.
-}

packetSize = blockSize - Crypto.msgHeaderLen - ARQ.headerLen

blockSize :: Int
blockSize = 16 * 1024 

-- arbitrary constants
padding = 69 -- pad bytes value
msgHead = 33 -- byte prefixing a a length prefixed message in the stream

recvPipe arq decrypt =
  DC.map decrypt =$ DC.catMaybes =$ arq
  =$ conduitGet (skipWhile (== padding) >> getMsg) =$ conduitGet get

sendPipe packetSize arq encrypt =
  DC.map (fmap $ encodeMsg) =$ isolateAndPad packetSize =$ arq =$ (encryptPipe encrypt)

{-
 read data and control messages with priority 
 given to control messages;
 pushes Nothing upstream if no queue has anything

-}
msgSource controlSend userSend = forever $ do
  item <- liftIO $ atomically $
    (fmap Just $
       ((fmap Control $ readTQueue controlSend) `orElse`
        (fmap Data $ readTQueue userSend)))
    `orElse` return Nothing
  DC.yield item

readSource read = forever $ do
  item <- read
  DC.yield item

-- filter messages into control and data
msgSink controlQ dataQ
  = awaitForever $ \case 
      (Data d) -> liftIO . atomically .writeTQueue dataQ $ d
      (Control c) -> liftIO . atomically . writeTQueue controlQ $ c

-- encryption

data Encrypter = Encrypter {
  runE :: ByteString -> (ByteString, Encrypter)
}

-- make an encrypter from an encryption function and a crpg
encrypter encrypt cprg = Encrypter {
  runE = \bs -> let (bytes, next) = cprgGenerate 16 cprg
          in (encrypt (fromRight $ DS.decode bytes :: Entropy) bs, encrypter encrypt next)
}

-- to deal with the fact that the encrypt pipe needs to go
-- through maybe values
-- TODO: this code is obscure - simplify it

encryptPipe encrypt = concatMapAccum
  (\mplain encrypt -> case mplain of
           Just plain -> let (cypher, next) = runE encrypt plain in (next, [Just cypher])
           Nothing -> (encrypt, [Nothing])) encrypt

{-
  wait for the bt stream to produce a piece,
  if you have any payload from upstream to send, send it.
  if not pass back the piece unharmed.
-}
outgoingSink getPiece putBack dataGate = do
  -- get a piece as it's about to leave the local bt client

  lift $ atomically $ goThroughGate dataGate

  piece <- lift getPiece
  upstream <- await 

  case upstream of
    (Just maybePayload) -> do
      lift $ putBack $ case maybePayload of
        (Just payload) -> payload 
        Nothing -> piece  -- move on, nothing to change
      outgoingSink getPiece putBack dataGate
    Nothing -> do
      lift $ putBack piece -- put back piece unharmed
      return () -- terminate sink

-- isolateAndPad :: Monad m => Int -> Conduit (Maybe BS.ByteString) m (Maybe BS.ByteString)
isolateAndPad n = do
  maybeBytes <- takeWhileSmth n
  case maybeBytes of 
    Nothing -> return () -- terminate
    Just lazyBytes -> do
      let bytes = BSL.toStrict lazyBytes
      yield $ if (BS.length bytes > 0) then Just $ pad bytes n padding else Nothing
      isolateAndPad n

pad bs targetLen padding = BS.concat [bs, BS.replicate (targetLen - BS.length bs) padding]


data Pipe r s = Pipe {
    pipeRecv :: TQueue r 
  , pipeSend :: TQueue s 
}

-- all channels and pipes for data grouped together
data DataPipes r s = DataPipes {
    controlPipe :: Pipe r s
  , pieceHooks :: PieceHooks
  -- used to bar the sending of data to remote
  -- until all control communication is done so no data leakage occurs
  , dataGate :: Gate
}

-- putting it all together
launchPipes packetSize  initARQ encrypter decrypt
            (DataPipes control (PieceHooks {..}) allowData) = do
  userSend <- liftIO $ (newTQueueIO :: IO (TQueue ByteString))
  userRecv <- liftIO $ (newTQueueIO :: IO (TQueue ByteString))

  -- create clock - keeps track of how many pieces flow upstream /downstream
  clock <- liftIO $ newClock 
  -- the clock ticks are associated with the functions handling the receive
  -- and sending of pieces respectively
  let clockedRecvPiece = liftIO $ do
                             piece <- atomically $ read recvPiece
                             tickRecv clock
                             return piece
  let clockedPutBackPiece p = liftIO $ (atomically $ write sendPutBack p) >> tickSend clock

  arq <- liftIO $ initARQ packetSize clock

  -- launch receive pipe
  allocLinkedAsync $ async
          $ (readSource clockedRecvPiece) =$ (recvPipe (recvARQ arq) decrypt)
          $$ msgSink (pipeRecv control) userRecv

  -- launch send pipe
  allocLinkedAsync $ async
         $ (msgSource (pipeSend control) userSend)
         =$ sendPipe packetSize (sendARQ arq) encrypter
         $$ outgoingSink (atomically $ read sendGetPiece) clockedPutBackPiece allowData

  return $ Pipe userRecv userSend

pipeToConnData pipe = ConnData {
      connSend = atomically . writeTQueue (pipeSend pipe)
    , connRecv = atomically $ readTQueue (pipeRecv pipe)
  }

-- messages 

-- a token given by the server to the client
-- for the client to recover its session
type SessionToken = ByteString
tokenLen = 64 -- bytes

-- the messages sent on the wire in their most general form
data WireMessage a = Data ByteString | Control a 
  deriving (Eq, Show)

data ServerMessage = AcceptConn SessionToken | RejectConn
  deriving (Eq, Show)

data ClientMessage = ConnRequest ByteString (Maybe SessionToken)
  deriving (Eq, Show)


-- TODO: replace the following with automatic derivation of Serialize
instance Serialize ServerMessage where
  put (AcceptConn token) = putWord8 0 >> putByteString token
  put RejectConn = putWord8 1

  get =     (byte 0 *> (AcceptConn <$> getBytes tokenLen))
        <|> (byte 1 *> return RejectConn)


instance Serialize ClientMessage where
  put (ConnRequest k maybeToken) = putWord8 0 >> putByteString k >> put maybeToken
  get = (byte 0 *> (ConnRequest <$> getBytes Crypto.keySize <*> get))


instance (Serialize a) => Serialize (WireMessage a) where
  put (Data bs) = putWord8 0 >> putByteString bs
  put (Control m) = putWord8 1 >> put m
  get =   (byte 0 *> (Data <$> getRemaining))
      <|> (byte 1 *> (Control <$> get)) 

-- encodes a custom serializable msg
encodeMsg :: Serialize a => a -> ByteString
encodeMsg = runPut . putMsg . DS.encode

-- length prefixed messages 
-- with a constant header (msgHead)
putMsg m = putWord8 msgHead >> putWord32le (fromIntegral $ BS.length m) >> putByteString m
getMsg = byte msgHead >> getWord32le >>= getBytes . fromIntegral


-- bittorrent stream handlers

data PieceHooks = PieceHooks {
    recvPiece :: SharedMem ByteString

  -- this is pretty horrible
  -- TODO make sure that this is in some way atomic aka
  -- you have a single operation applyToPiece :: (Piece -> IO (Piece)) -> IO ()
  -- or better MonadIO m => (Piece -> m Piece) -> m ()
  -- and an exception occurs in the operation the piece is pushed back out unharmed
  , sendGetPiece :: SharedMem ByteString
  , sendPutBack :: SharedMem ByteString
}

-- a wrapper for shared memory (can be tvar, or tbqueue or whatever)
data SharedMem a = SharedMem { read :: STM a
                             , write :: a -> STM ()
                             , tryRead :: STM (Maybe a)}

stmShared r w tr var = SharedMem { read = r var
                              , write = w var
                              , tryRead = tr var }

makePieceHooks = do
  [sendGet, sendPut] <- CM.replicateM 2 (liftIO $ newEmptyTMVarIO)
  recv <- newTQueueIO
  return $ PieceHooks (stmShared readTQueue writeTQueue tryReadTQueue recv)
                      (stmShared takeTMVar putTMVar tryTakeTMVar sendGet)
                      (stmShared takeTMVar putTMVar tryTakeTMVar sendPut)

makeStreams (PieceHooks {..}) getFileFixer = do
  -- a tmvar used to notify the recv thread of the infohash of the stream
  -- in the case in which the send thread learns about it
  return $ (btStreamHandler $ recvStream getFileFixer (atomically . write recvPiece)
           , btStreamHandler $ sendStream (liftIO . atomically . (write sendGetPiece))
                                   (liftIO $ atomically $ read sendPutBack))

-- the stream handler stops parsing when it can no longer 
-- make sense of the stream and instead forwards Unparsed ByteStrings
-- which are simply proxied
btStreamHandler transform = (mkConduitGet handleStreamFail (get :: Get BT.StreamChunk)
                             >> DC.map BT.Unparsed)
                          =$ transform
                          =$ conduitPut (put :: Putter BT.StreamChunk)

handleStreamFail _ = DC.map BT.Unparsed

sendStream putPiece getPiece 
  = chunkStream (\ih -> loop)
    where
      loop = awaitForPiece $ \p -> do
--               debugM logger "got a piece to send out"
               putPiece (BT.block p)
               updatedP <- getPiece 
               assert (BS.length updatedP == BS.length (BT.block p)) (return ())
               return $ (\b -> p {BT.block = b}) updatedP

recvStream :: (InfoHash -> IO (Maybe BlockLoader))
              -> (ByteString -> IO ()) 
              -> ConduitM BT.StreamChunk BT.StreamChunk IO ()
recvStream getBlockLoader putRecv
  = chunkStream (loop . hsInfoHash)
    where
      loop ih = do
        liftIO $ debugM logger $ " *** the stream has infohash " P.++ (show ih)
        maybeFixPiece <- liftIO $ getBlockLoader ih
        case maybeFixPiece of
          Nothing -> do
            liftIO $ debugM logger " ***there's no piece fixer. just proxy things"
            awaitForever (\m -> DC.yield m) -- just proxy stuff
          Just blockLoader -> do
            liftIO $ debugM logger "*** receiving bitsmuggler tampered-pieces"
            awaitForPiece $ \piece -> do
                              putRecv $ BT.block piece

                              -- TODO: don't fix it if it ain't broken
                               -- pieces that are not tampered with should not be fixed
                              fixed <- fixPiece piece (loadBlock blockLoader)
                              return fixed
            liftIO $ closeLoader blockLoader

fixPiece p@(BT.Piece {..}) loadBlock = do
  goodBlock <- loadBlock (index, BT.Block {BT.blockOffset = begin,
                                        BT.blockSize  = BS.length block})
  return $ p {BT.block = goodBlock}


chunkStream postHandshake = do
  upstream <- await
  case upstream of
    Just hs@(BT.HandShake _ _ _) -> do
      liftIO $ debugM logger $ "parsed handshake " ++ (show hs)
      leftover hs
      postHandshake hs

    -- this is to treat the case in which it's not a bittorrent connection
    -- we are just proxying unparsed chunks;
    Just other -> do
      DC.yield other
      chunkStream postHandshake 
     
    Nothing -> return ()

hsInfoHash (BT.HandShake _ ih _) = ih

awaitForPiece :: (BT.Message -> IO BT.Message) 
              -> (ConduitM BT.StreamChunk BT.StreamChunk IO ())
awaitForPiece f
  = awaitForever
      (\case
        (BT.MsgChunk sz p@(BT.Piece {..})) -> (liftIO $ f p) >>= DC.yield . (BT.MsgChunk sz)
        other -> DC.yield other)

-- conduit extras

-- take n bytes OR until Nothing is encountered
-- based on the code of 'take' from Data.Conduit.Binary

takeWhileSmth :: Monad m => Int -> Consumer (Maybe BS.ByteString) m (Maybe BSL.ByteString)
takeWhileSmth  0 = return $ Just BSL.empty
takeWhileSmth n0 = go n0 id
  where
    go n front =
        await >>= maybe (return $ bsToMaybe $ BSL.fromChunks $ front []) go'
      where
        go' mbs = case mbs of 
          Nothing -> return $ Just $ BSL.fromChunks $ front []
          Just bs -> case BS.length bs `compare` n of
                LT -> go (n - BS.length bs) (front . (bs:))
                EQ -> return $ Just $ BSL.fromChunks $ front [bs]
                GT ->
                    let (x, y) = BS.splitAt n bs
                     in assert (not $ BS.null y) $ leftover (Just y) >> return (Just $ BSL.fromChunks $ front [x])

bsToMaybe bs = if BSL.length bs == 0 then Nothing else (Just bs)

-- cereal extras
skipWhile p = do 
  w <- lookAhead getWord8
  if p w then skip 1 >> skipWhile p else return ()

-- stm extras 

-- used to stop a thread until some other thread opens the gate
type Gate = TMVar ()

-- starts out closed
newGate :: IO Gate
newGate = newEmptyTMVarIO

-- blocks if the gate is closed
goThroughGate g = readTMVar g

-- these block if the gate is already in the right state open/closed
openGate g = putTMVar g ()
closeGate g = takeTMVar g

