{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Network.BitSmuggler.Protocol where

import Prelude as P
import Data.Maybe
import Data.Serialize as DS
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Conduit.Cereal
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.ByteString as BS
import Data.Word
import Control.Monad
import Control.Applicative hiding (empty)
import Control.Exception
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Concurrent.Async

import Network.BitSmuggler.Crypto as Crypto
import Network.BitSmuggler.Utils
import Network.BitSmuggler.BitTorrentParser as BT

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
blockSize :: Int
blockSize = 16 * 1024 

-- arbitrary constants
padding = 69 -- pad bytes value
msgHead = 33 -- byte prefixing a a length prefixed message in the stream

recvPipe arq decrypt =
  decryptPipe decrypt =$ DC.catMaybes =$ arq
  =$ conduitGet (skipWhile (== padding) >> getMsg) =$ conduitGet get


sendPipe packetSize arq encrypt =
  DC.map (fmap $ encodeMsg) =$ isolateAndPad packetSize =$ arq =$ (encryptPipe encrypt)

tryQueueSource q = forever $ do
  item <- liftIO $ atomically $ tryReadTQueue q
  DC.yield item

queueSource q = forever $ do
  item <- liftIO $ atomically $ readTQueue q
  DC.yield item

queueSink q = awaitForever (liftIO . atomically . writeTQueue q)

-- encryption

data Encrypter = Encrypter {
  runE :: ByteString -> (ByteString, Encrypter)
}

data Decrypter = Decrypter {
  runD :: ByteString -> Maybe (ByteString, Decrypter)
}

-- to deal with the fact that the encrypt pipe needs to go
-- through maybe values
-- TODO: this code is obscure - simplify it

encryptPipe encrypt = concatMapAccum
  (\mplain encrypt -> case mplain of
           Just plain -> let (cypher, next) = runE encrypt plain in (next, [Just cypher])
           Nothing -> (encrypt, [Nothing])) encrypt

decryptPipe decrypt = concatMapAccum
  (\cypher decrypt -> case runD decrypt cypher of
           Just (plain, next) -> (next, [Just plain])
           Nothing -> (decrypt, [Nothing])) decrypt 

{-
  wait for the bt stream to produce a piece,
  if you have any payload from upstream to send, send it.
  if not pass back the piece unharmed.
-}
outgoingSink getPiece putBack = do
  -- get a piece as it's about to leave the local bt client
  piece <- getPiece
  upstream <- await 
  case upstream of
    (Just maybePayload) -> do
      putBack $ case maybePayload of
        (Just payload) -> payload 
        Nothing -> piece  -- move on, nothing to change
      outgoingSink getPiece putBack
    Nothing -> return ()


isolateAndPad :: Monad m => Int -> Conduit (Maybe BS.ByteString) m (Maybe BS.ByteString)
isolateAndPad n = forever $ do
  bytes <- fmap BS.concat $ isolateWhileSmth n =$ DC.consume
  yield $ if (BS.length bytes > 0) then Just $ pad bytes n padding else Nothing

pad bs targetLen padding = BS.concat [bs, BS.replicate (targetLen - BS.length bs) padding]

-- messages 

data ServerMessage = ServerData ByteString | AcceptConn | RejectConn

data ClientMessage = ClientData ByteString | ConnRequest ByteString -- the pub key

-- TODO: replace the following with automatic derivation of Serialize
instance Serialize ServerMessage where
  put AcceptConn = putWord8 0
  put RejectConn = putWord8 1
  put (ServerData bs) = putWord8 2 >> putByteString bs

  get = (byte 0 *> return AcceptConn) <|> (byte 1 *> return RejectConn)
        <|> (byte 2 *> (ServerData <$> getRemaining))

instance Serialize ClientMessage where
  put (ConnRequest k) = putWord8 0 >> put k
  put (ClientData bs) = putWord8 1 >> putByteString bs
  get = (byte 0 *> (ConnRequest <$> getRemaining))
        <|> (byte 1 *> (ClientData <$> getRemaining))

-- encodes a custom serializable msg
encodeMsg :: Serialize a => a -> ByteString
encodeMsg = runPut . putMsg . DS.encode

-- length prefixed messages 
-- with a constant header (msgHead)
putMsg m = putWord8 msgHead >> putWord32le (fromIntegral $ BS.length m) >> putByteString m
getMsg = byte msgHead >> getWord32le >>= getBytes . fromIntegral



-- bittorrent stream handlers

data PieceHooks = PieceHooks {
    recvPiece :: TQueue ByteString
  , sendGetPiece :: TQueue ByteString
  , sendPutBack :: TQueue ByteString
}


makeStreams (PieceHooks {..}) getFileFixer = do
  notifyIH <- newTBQueueIO 1 -- get info hash across to the other stream
  return $ (btStreamHandler $ recvStream getFileFixer
                                   (liftIO . atomically . writeTQueue recvPiece)
                                   (liftIO $ atomically $ readTBQueue notifyIH)
           , btStreamHandler $ sendStream  (liftIO . atomically . writeTQueue sendGetPiece)
                                   (liftIO $ atomically $ readTQueue sendPutBack)
                                   (liftIO . atomically . writeTBQueue notifyIH))

btStreamHandler transform = conduitGet (get :: Get StreamChunk)
                          =$ transform
                          =$ conduitPut (put :: Putter StreamChunk)

type LoadBlock = (Int, Block) -> ByteString 

sendStream putPiece getPiece notifyIH
  = chunkStream (\hs -> (notifyIH $ hsInfoHash hs) >> loop) loop
    where
      loop = awaitForPiece $ \p -> putPiece (block p)
                                   >> fmap (\b -> p {block = b}) getPiece 

recvStream :: (InfoHash -> IO (Maybe LoadBlock))
              -> (ByteString -> IO ()) -> (IO InfoHash)
              -> ConduitM StreamChunk StreamChunk IO ()
recvStream getBlockLoader putRecv readIH
  = chunkStream (loop . hsInfoHash) ((liftIO $ readIH) >>= loop)
    where
      loop ih = do
        maybeFixPiece <- liftIO $ getBlockLoader ih
        case maybeFixPiece of
          Nothing -> awaitForever (\m -> DC.yield m) -- just proxy stuff
          Just loadBlock ->
            awaitForPiece $ \piece -> do
                              putRecv $ block piece
                              -- TODO: don't fix it if it ain't broken
                               -- pieces that are not tampered with should not be fixed
                              return $ fixPiece piece loadBlock

fixPiece p@(Piece {..}) loadBlock =
  let goodBlock = loadBlock (index, Block {blockOffset = begin,
                                        blockSize  = BS.length block})
  in p {block = goodBlock}


chunkStream onHandshake otherwise = do
  upstream <- await
  case upstream of
    Just hs@(HandShake _ _ _) -> leftover hs >> onHandshake hs
    Just other -> leftover other >> otherwise
    Nothing -> return ()

hsInfoHash (HandShake _ ih _) = ih

awaitForPiece :: (BT.Message -> IO BT.Message) 
              -> (ConduitM StreamChunk StreamChunk IO ())
awaitForPiece f
  = awaitForever
      (\case
        (MsgChunk sz p@(Piece {..})) -> (liftIO $ f p) >>= DC.yield . (MsgChunk sz)
        other -> DC.yield other)

-- conduit extras

-- isolate n bytes OR until Nothing is encountered
-- based on the code of 'isolate' from Data.Conduit.Binary
isolateWhileSmth :: Monad m
        => Int
        -> Conduit (Maybe BS.ByteString) m BS.ByteString
isolateWhileSmth =
    loop
  where
    loop 0 = return ()
    loop count = do
        mbs <- await
        case mbs of
            Nothing -> return ()
            Just Nothing -> return ()
            Just (Just bs) -> do
                let (a, b) = BS.splitAt count bs
                case count - BS.length a of
                    0 -> do
                        unless (BS.null b) $ leftover $ Just b
                        DC.yield a
                    count' -> assert (BS.null b) $ DC.yield a >> loop count'

-- cereal extras
skipWhile p = do 
  w <- lookAhead getWord8
  if p w then skip 1 >> skipWhile p else return ()

