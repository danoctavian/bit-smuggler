{-# LANGUAGE OverloadedStrings #-}

module Network.BitSmuggler.BitTorrentParser
where

import Prelude as P
import Control.Applicative hiding (empty)
import Control.Monad as CM

import Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Serialize as DS
import Data.Serialize.Put 
import Data.Serialize.Get as DS
import Data.Word
import Data.LargeWord
import Data.Char

import Data.Conduit as DC
import Data.Conduit.List as DCL
import Data.ByteString.Char8 as BSC

import Network.BitSmuggler.Utils

-- TODO: remove
import Data.Conduit.Cereal
import Data.Conduit.Binary
import Control.Monad.Trans.Resource

type BitField    = BSL.ByteString
type PieceLength = Int

type PieceNum = Int

type BlockSize = Int
 
data Block = Block { blockOffset :: Int        -- ^ offset of this block within the piece
                   , blockSize   :: BlockSize  -- ^ size of this block within the piece
                   } deriving (Eq, Ord, Show)

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum
             | BitField BitField
             | Request PieceNum Block
             | Piece {index :: PieceNum, begin :: Int, block :: BS.ByteString}
             | Cancel PieceNum Block
             | Port Integer
             | Unknown ByteString -- covering all other unknown protocol fields
  deriving (Eq, Show)


-- the bittorrent stream starts with a header and continues with 
-- a series of length-prefixed messages
data StreamChunk = HandShake Word64 InfoHash PeerID | MsgChunk Word32 Message
  deriving (Show, Eq)

type PeerID = Word160

-- | The Protocol header for the Peer Wire Protocol
protocolHeader = "BitTorrent protocol"
protocolHeaderSize = BS.length protocolHeader

p8 :: Word8 -> Put
p8 = putWord8

p32be :: Integral a => a -> Put
p32be = putWord32be . fromIntegral

instance Serialize StreamChunk where
  put (HandShake extensions ih pid)
    = putWord8 (fromIntegral protocolHeaderSize) >> putByteString protocolHeader
    >> putWord64be extensions >> DS.put ih >> DS.put pid
  put (MsgChunk len m) = putWord32be len >> put m
  get = getHandShake <|> getMsgChunk  

getHandShake = byte (fromIntegral protocolHeaderSize) *> byteString protocolHeader 
         *> (HandShake <$> getWord64be <*> get <*> get)
getMsgChunk = do
  len <- getWord32be
  MsgChunk len <$> (DS.isolate (fromIntegral len) get)



instance Serialize Message where
  put KeepAlive       = return ()
  put Choke           = p8 0
  put Unchoke         = p8 1
  put Interested      = p8 2
  put NotInterested   = p8 3
  put (Have pn)       = p8 4 *> p32be pn
  put (BitField bf)   = p8 5 *> putLazyByteString bf
  put (Request pn (Block os sz))
                      = p8 6 *> CM.mapM_ p32be [pn,os,sz]
  put (Piece pn os c) = p8 7 *> CM.mapM_ p32be [pn,os] *> putByteString c
  put (Cancel pn (Block os sz))
                      = p8 8 *> CM.mapM_ p32be [pn,os,sz]
  put (Port p)        = p8 9 *> (putWord16be . fromIntegral $ p)
  put (Unknown content) = putByteString content
  
  get =  getKA      <|> getChoke
     <|> getUnchoke <|> getIntr
     <|> getNI      <|> getHave
     <|> getBF      <|> getReq
     <|> getPiece   <|> getCancel
     <|> getPort    <|> getUnknown

getChoke   = byte 0 *> return Choke
getUnchoke = byte 1 *> return Unchoke
getIntr    = byte 2 *> return Interested
getNI      = byte 3 *> return NotInterested
getHave    = byte 4 *> (Have <$> gw32)
getBF      = byte 5 *> (BitField <$> (remaining >>= getLazyByteString . fromIntegral))
getReq     = byte 6 *> (Request  <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPiece   = byte 7 *> (Piece    <$> gw32
                                 <*> gw32
                                 <*> (remaining >>= getByteString))
getCancel  = byte 8 *> (Cancel   <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPort    = byte 9 *> (Port . fromIntegral <$> getWord16be)
getUnknown = Unknown<$> (remaining >>= getByteString)

getKA      = do
    empty <- isEmpty
    if empty
        then return KeepAlive
        else fail "Non empty message - not a KeepAlive"

gw32 :: Integral a => Get a
gw32 = fromIntegral <$> getWord32be

toBS :: String -> BS.ByteString
toBS = BS.pack . P.map toW8

toW8 :: Char -> Word8
toW8 = fromIntegral . ord

runTestParse = do
  let testFile = "../testdata/incomingBTTraffic"
  chunks <- runResourceT $ sourceFile testFile
               =$ conduitGet (get :: Get StreamChunk) $$ DCL.take 10000
  P.putStrLn (show $ P.length chunks)
  return ()

