{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Network.BitSmuggler.Utils (
    localhost
  , milli 
  , getRemaining
  , eitherToMaybe
  , fromRight
  , try'
  , if'
  , sourceTQueue
  , sinkTQueue
  , conduitBytes
  , alwaysRetry
  , allocAsync
  , allocLinkedAsync
  , AsyncRes
  , portNumFixEndian
  , PortNum
  , InfoHash
  , binPut
  , binGet
  , byte
  , byteString
  , BitSmugglerException (..)
  , hoistMaybe
  , toLazy
  , logger
  , absolutePath
  , maybeRead
) where

import Data.Typeable
import Data.Byteable
import Data.Maybe
import qualified Data.Binary as Bin
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.LargeWord
import Data.Serialize as DS
import Data.Serialize.Put as DS
import Data.Serialize.Get as DS
import System.FilePath.Posix
import System.Directory

import Data.Conduit as DC
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Control.Retry
import qualified Control.Monad.Catch as Catch
import Network.BitTorrent.Types (PortNum, InfoHash)

-- bitsmuggler logger

logger = "BitSmuggler-logger"

-- exceptions 

-- TODO: move this to a more appropriate place
data BitSmugglerException = UnsupportedFeature | TorrentFileIntegrityFail
                          | ClientProtocolError | PeerProtocolError 
  deriving (Show, Typeable)

instance Exception BitSmugglerException

-- constants

localhost = "127.0.0.1"

milli :: Int
milli = 10 ^ 6

instance Byteable Word128 where
  toBytes = BSL.toStrict . Bin.encode

instance Serialize Word128 where
  get = binGet 16
  put = binPut

instance Byteable Word256 where
  toBytes = BSL.toStrict . Bin.encode

instance Serialize Word256 where
  get = binGet 32
  put = binPut

-- just because i made the infohash a word160...
-- i got this ugly thing
instance Serialize InfoHash where
  get = binGet 20
  put = binPut 


-- CEREAL 

-- given a type that has a Binary instance
-- and has a constant size serialized 
-- write cereal put and get in terms of that
binGet sz = do
    bs <- getBytes sz
    case Bin.decodeOrFail $ BSL.fromChunks [bs] of
      Right (_, _, v) -> return v
      Left _ -> fail "shit son"
binPut :: (Bin.Binary a) => a -> DS.Put
binPut = DS.putLazyByteString . Bin.encode

getRemaining = remaining >>= getBytes

byte :: Word8 -> Get Word8
byte w = do
    x <- lookAhead getWord8
    if x == w then getWord8
              else fail $ "Expected byte: '" ++ show w ++ "' got: '" ++ show x ++ "'"

byteString :: ByteString -> Get ByteString
byteString bs = do
  ahead <- lookAhead (getByteString $ BS.length bs)
  if ahead == bs then getByteString $ BS.length bs
                 else fail $ "Expected bs: '" ++ show bs ++ "' got: '" ++ show ahead ++ "'"
  

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

hoistMaybe :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybe = maybe mzero return

fromRight (Right v) = v

if' c a b = if c then a else b

-- conduit

sourceTQueue :: MonadIO m => TQueue a -> Source m a
sourceTQueue chan = forever $ (liftIO $ atomically $ readTQueue chan) >>= DC.yield

sinkTQueue :: MonadIO m => TQueue a -> Consumer a m ()
sinkTQueue queue = awaitForever (\item -> liftIO $ atomically $ writeTQueue queue item)

conduitBytes :: Monad m =>  Conduit ByteString m Word8 
conduitBytes = awaitForever (\bs -> forM (BS.unpack bs) DC.yield)


-- PortNum from Network.Socket always assumes that whatever value
-- passed into the constructor is a big-endian (newtwork byte order)
-- call this on the value passed in to make it get what you actually mean
portNumFixEndian = (\(Right v) -> v) . runGet getWord16be  . runPut . putWord16host

try' :: IO a -> IO (Either SomeException a)
try' = try

alwaysRetry n = Catch.Handler $ \ (e :: SomeException) -> return True

-- ASYNC

 -- async resource
type AsyncRes a = (ReleaseKey, Async a)
allocAsync runAsync = allocate runAsync (liftIO . cancel)

allocLinkedAsync runAsync
  = allocate ( do
      a <- runAsync
      link a
      return a) (liftIO . cancel)

-- read
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- BYTESTRING
toLazy bs = BSL.fromChunks [bs]


-- FILESYSTEM

-- WARNING: doesn't deal with dot dots
absolutePath path = fmap ((flip (</>)) path) getCurrentDirectory
