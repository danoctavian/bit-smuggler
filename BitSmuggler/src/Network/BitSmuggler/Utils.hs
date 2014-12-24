{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.BitSmuggler.Utils where

import Data.Byteable
import Data.Binary as Bin
import qualified Data.ByteString.Lazy as BSL
import Data.LargeWord
import Data.Serialize
import Data.Conduit as DC
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class
import Control.Exception
import Control.Monad

instance Byteable Word128 where
  toBytes = BSL.toStrict . Bin.encode

instance Byteable Word256 where
  toBytes = BSL.toStrict . Bin.encode

getRemaining = remaining >>= getBytes

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

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
