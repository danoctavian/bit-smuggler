module Network.BitSmuggler.BitTorrentParser
where

import Prelude as P
import Control.Applicative hiding (empty)
import Control.Monad as CM

import Data.Bits
import Data.Maybe as DM
import Data.Monoid
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get

import Data.Attoparsec as DA
import Data.Attoparsec.Combinator as DACo
import Data.Attoparsec.Char8 as DAC
import Data.Attoparsec.Binary

import Data.Word
import Data.Char
import System.IO

import Data.Conduit as DC
import Data.Conduit.List as DCL
import Data.ByteString.Char8 as DBC

import Network.BitSmuggler.Utils

type BitField    = DBL.ByteString
type PieceLength = Int

-- copied types
type PieceNum = Int
type InfoHash = DBL.ByteString

type BlockSize = Int
 
data Block = Block { blockOffset :: Int        -- ^ offset of this block within the piece
                   , blockSize   :: BlockSize  -- ^ size of this block within the piece
                   } deriving (Eq, Ord, Show)

--type LogChannel = Channel LogMsg


data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum -- Int
             | BitField BitField
             | Request PieceNum Block
             | Piece {index :: PieceNum, begin :: Int, block :: DB.ByteString}
             | Cancel PieceNum Block
             | Port Integer
             | Handshake  ([Capabilities], ByteString, ByteString) ByteString -- hacky; TODO: refactor later
  deriving (Eq, Show)


data HandShake = HandShake 
                  String -- Protocol Header
                  Word64 -- Extension Bias

-- | The Protocol header for the Peer Wire Protocol
protocolHeader :: String
protocolHeader = "BitTorrent protocol"

extensionBasis :: Word64
extensionBasis = 0

extensionFast :: Word64
extensionFast = 4

p8 :: Word8 -> Put
p8 = putWord8

p32be :: Integral a => a -> Put
p32be = putWord32be . fromIntegral

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
    put (Handshake _ raw) = putByteString raw
    
    get =  getKA      <|> getChoke
       <|> getUnchoke <|> getIntr
       <|> getNI      <|> getHave
       <|> getBF      <|> getReq
       <|> getPiece   <|> getCancel
       <|> getPort

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
getKA      = do
    empty <- isEmpty
    if empty
        then return KeepAlive
        else fail "Non empty message - not a KeepAlive"

gw32 :: Integral a => Get a
gw32 = fromIntegral <$> getWord32be
byte :: Word8 -> Get Word8
byte w = do
    x <- lookAhead getWord8
    if x == w
        then getWord8
        else fail $ "Expected byte: '" ++ show w ++ "' got: '" ++ show x ++ "'"


protocolHeaderSize = P.length protocolHeader

toBS :: String -> DB.ByteString
toBS = DB.pack . P.map toW8

toW8 :: Char -> Word8
toW8 = fromIntegral . ord

headerParser :: Parser(Either ByteString Message)

headerParser = do
    let protoLen = fromIntegral $ P.length protocolHeader
    let bsProtoHead = DBC.pack protocolHeader
    DA.word8 protoLen
    string bsProtoHead
    caps <- anyWord64be
    let magicLen = 20
    ihR <- DA.take magicLen
    pid <- DA.take magicLen
    return $ Right $ Handshake (decodeCapabilities caps, ihR, pid) $
            DB.concat $ [DB.pack [protoLen], bsProtoHead, encode caps, ihR, pid]


-- TODO: there's more
data Capabilities = Fast | DHT | ExtensionProtocol | ExtensionNegotiationProtocol
    deriving (Show, Eq)

extensionBits = [(Fast, 62), (ExtensionProtocol, 44), (DHT, 64)]    
-- 
decodeCapabilities :: Word64 -> [Capabilities]
decodeCapabilities w64
    = DM.catMaybes $ P.map (\(ext, bit) -> if testBit w64 (64 - bit) then Just ext else Nothing) extensionBits

findCapabilities :: Word64 -> [Int]
findCapabilities w64 =
  DM.catMaybes $ P.map (\bit -> if testBit w64 (64 - bit) then Just bit else Nothing) [0..63]

packageParser :: Parser (Either ByteString Message)
packageParser = do
  packLen <- fmap (\w -> fromIntegral w :: Int) anyWord32be
  fmap decodeOrLeave $ DA.take packLen
  where
    decodeOrLeave bs = case decode bs of
                        (Left _) -> Left bs
                        (Right m) -> Right m

runTestParse = do
    bs <- DB.readFile "../testdata/incomingBTTraffic"
    P.putStrLn $ show $ DB.take 100 bs
    P.putStrLn $ show $ DB.take (72 + 218 + 4 + 1 + 4 + 3 + 4 + 3 + 4 + 1) bs
    P.putStrLn $ show $ (\(DAC.Done _ r) ->  (P.map (DB.length . block . fromRight) $ P.filter isP r,  P.length r) ) $ 
                        DA.parse parseInOrder bs

--(\(DAC.Done _ [Right (Handshake _ bs)]) -> DA.parse headerParser bs) $



serializePackage pack = case pack of
    (Handshake _ _) -> encode pack
    other -> prefixLen . encode $ pack


prefixLen bs =  DB.concat [encode $ (\l -> fromIntegral l :: Word32) $ DB.length bs, bs]

parseInOrder = headerParser >> (DACo.count 400 packageParser)
parseWith2 = DACo.count 1 (headerParser <|> packageParser)

isP (Right (Piece _ _ _)) = True
isP _ = False


