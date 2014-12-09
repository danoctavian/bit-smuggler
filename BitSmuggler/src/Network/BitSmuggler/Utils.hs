{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.BitSmuggler.Utils where

import Data.Byteable
import Data.Binary as Bin
import qualified Data.ByteString.Lazy as BSL
import Data.LargeWord
import Data.Serialize

instance Byteable Word128 where
  toBytes = BSL.toStrict . Bin.encode

instance Byteable Word256 where
  toBytes = BSL.toStrict . Bin.encode

getRemaining = remaining >>= getBytes

eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right v) = Just v

if' c a b = if c then a else b
