module Network.BitSmuggler.Protocol where

import Prelude as P
import Data.Serialize as DS
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Conduit.Cereal
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.ByteString as BS
import Data.Word
import Control.Monad
import Control.Exception
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Concurrent.Async

import Network.BitSmuggler.Crypto as Crypto
import Network.BitSmuggler.ARQ as ARQ



{-

handling data streams
-}

-- the size of a packet embedded in a bittorrent piece message
packetSize = blockSize - Crypto.msgHeaderLen - ARQ.headerLen

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
blockSize = 16 * 1024 

-- arbitrary constants
padding = 69 -- pad bytes value
msgHead = 33 -- byte prefixing a a length prefixed message in the stream

recvPipe arq decrypt =
  decrypt =$ arq =$ conduitGet (skipWhile (== padding) >> getMsg) =$ conduitGet get


sendPipe packetSize arq encrypt =
  DC.map (fmap $ encodeMsg) =$ isolateAndPad packetSize =$ arq =$ (encryptPipe encrypt)

tryQueueSource q = forever $ do
  item <- liftIO $ atomically $ tryReadTQueue q
  DC.yield item


data Encrypter = Encrypter {
  run :: ByteString -> (ByteString, Encrypter)
}

-- to deal with the fact that the encrypt pipe needs to go
-- through maybe values
encryptPipe encrypt = mapAccum
  (\mplain encrypt -> case mplain of
           Just plain -> let (cypher, next) = run encrypt plain in (next, Just cypher)
           Nothing -> (encrypt, Nothing)) encrypt
{-
  wait for the bt stream to produce a piece
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
        (Just payload) -> maybePayload
        Nothing -> piece  -- move on, nothing to change
      outgoingSink getPiece putBack
    Nothing -> return ()


isolateAndPad :: Monad m => Int -> Conduit (Maybe BS.ByteString) m (Maybe BS.ByteString)
isolateAndPad n = forever $ do
  bytes <- fmap BS.concat $ isolateWhileSmth n =$ DC.consume
  yield $ if (BS.length bytes > 0) then Just $ pad bytes n padding else Nothing

pad bs targetLen padding = BS.concat [bs, BS.replicate (targetLen - BS.length bs) padding]

-- messages 

data ServerMessage = ServerData ByteString | AcceptConn

data ClientMessage = ClientData ByteString | ConnRequest Key

-- encodes a custom serializable msg
encodeMsg :: Serialize a => a -> ByteString
encodeMsg = runPut . putMsg . DS.encode

-- length prefixed messages 
-- with a constant header (msgHead)
putMsg m = putWord8 msgHead >> putWord32le (fromIntegral $ BS.length m) >> putByteString m
getMsg = byte msgHead >> getWord32le >>= getBytes . fromIntegral

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

byte :: Word8 -> Get Word8
byte w = do
    x <- lookAhead getWord8
    if x == w then getWord8
              else fail $ "Expected byte: '" ++ show w ++ "' got: '" ++ show x ++ "'"

