{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Network.BitSmuggler.Utils (
    localhost
  , milli 
  , getRemaining
  , eitherToMaybe
  , fromRight
  , try'
  , if'
  , sourceTChan
  , alwaysRetry
  , allocAsync
  , allocLinkedAsync
  , AsyncRes
  , portNumFixEndian
  , PortNum
  , InfoHash
  , binPut
  , binGet
) where

import Data.Byteable
import Data.Binary as Bin
import qualified Data.ByteString.Lazy as BSL
import Data.LargeWord
import Data.Serialize as DS
import Data.Serialize.Put as DS
import Data.Serialize.Get as DS

import Data.Conduit as DC
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Resource
import Control.Concurrent.Async
import Control.Retry
import qualified Control.Monad.Catch as Catch
import Network.BitTorrent.ClientControl (PortNum, InfoHash)

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

-- given a type that has a Binary instance
-- and has a constant size serialized 
-- write cereal put and get in terms of that
binGet sz = do
    bs <- getBytes sz
    case Bin.decodeOrFail $ BSL.fromChunks [bs] of
      Right (_, _, v) -> return v
      Left _ -> fail "shit son"
binPut :: (Binary a) => a -> DS.Put
binPut = DS.putLazyByteString . Bin.encode

getRemaining = remaining >>= getBytes

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

fromRight (Right v) = v

if' c a b = if c then a else b

-- conduit

sourceTChan :: MonadIO m => TChan a -> Source m a
sourceTChan chan = forever $ (liftIO $ atomically $ readTChan chan) >>= DC.yield

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

