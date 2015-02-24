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

data Message = Piece {index :: PieceNum, begin :: Int, block :: BS.ByteString}
             | Unknown ByteString -- covering all other uninteresting protocol messages
  deriving (Eq, Show)


-- the bittorrent stream starts with a header and continues with 
-- a series of length-prefixed messages
data StreamChunk = HandShake Word64 InfoHash PeerID | MsgChunk Word32 Message
                 | Unparsed ByteString -- an unparsed piece
  deriving (Show, Eq)

type PeerID = Word160

-- | The Protocol header for the Peer Wire Protocol
protocolHeader = "BitTorrent protocol"
protocolHeaderSize = BS.length protocolHeader

p32be :: Integral a => a -> Put
p32be = putWord32be . fromIntegral

instance Serialize StreamChunk where
  put (HandShake extensions ih pid)
    = putWord8 (fromIntegral protocolHeaderSize) >> putByteString protocolHeader
    >> putWord64be extensions >> DS.put ih >> DS.put pid
  put (MsgChunk len m) = putWord32be len >> put m
  put (Unparsed bs) = putByteString bs
  get = getHandShake <|> getMsgChunk  

getHandShake = byte (fromIntegral protocolHeaderSize) *> byteString protocolHeader 
         *> (HandShake <$> getWord64be <*> get <*> get)
getMsgChunk = do
  len <- getWord32be
  when (len > 17000) $ fail "too big"
  MsgChunk len <$> (DS.isolate (fromIntegral len) get)

instance Serialize Message where
  put (Piece pn os c) = putWord8 7 *> CM.mapM_ p32be [pn,os] *> putByteString c
  put (Unknown content) = putByteString content
  
  get =  getPiece <|> getUnknown

getPiece   = byte 7 *> (Piece    <$> gw32
                                 <*> gw32
                                 <*> (remaining >>= getByteString))
getUnknown = Unknown <$> (remaining >>= getByteString)

gw32 :: Integral a => Get a
gw32 = fromIntegral <$> getWord32be

runTestParse = do
  let testFile = "test-data/seedClientCapture"
  chunks <- runResourceT $ sourceFile testFile
               =$ conduitGet (get :: Get StreamChunk) $$ DCL.take 40
  P.putStrLn (show $ P.length $ P.filter isPiece chunks) -- P.all (== 16384) $ P.map getBlock $ P.filter isPiece chunks)
  return ()

isPiece (MsgChunk _ (Piece  _ _ _)) = True
isPiece _ = False
getBlock (MsgChunk _ p@(Piece _ _ _)) = BS.length $ block p
