{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BitSmuggler.TorrentFile (
    hashPieces
  , computeInfoHash
  , textToInfoHash
) where

import Crypto.Hash.SHA1 as SHA1
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Conduit.Binary as DCB
import Data.Conduit as DC
import Data.Conduit.List
import Control.Monad
import Prelude as P
import Control.Monad.Trans.Resource
import Data.Torrent
import Data.Map as M
import Data.BEncode
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as T

import Data.ByteString.Base16 as Base16

{-

functions for working with torrented files

-}

-- compute the hashes for the pieces of all the files of a torrent
hashPieces :: [FilePath] -> Int -> IO [ByteString]
hashPieces files pieceSize
  = runResourceT $ (P.foldl (>>) (return ()) $ P.map sourceFile files)
                    =$ hashEvery pieceSize $$ consume

hashEvery pieceSize = do
  piece <- fmap BSL.toStrict $ DCB.take pieceSize
  when (BS.length piece > 0) $ (DC.yield $ SHA1.hash piece) >> hashEvery pieceSize

computeInfoHash :: Torrent -> ByteString
computeInfoHash
  = SHA1.hash . BSL.toStrict . bPack
    . (\(BDict d) -> fromJust $ M.lookup "info" d) . serializeTorrent


textToInfoHash :: Text -> Maybe ByteString 
textToInfoHash text
    | hashLen == 40 = if BS.length inv == 0 then Just ihStr else Nothing
    | otherwise = Nothing
  where
    hashLen = BS.length hashStr
    hashStr = T.encodeUtf8 text
    (ihStr, inv) = Base16.decode hashStr
