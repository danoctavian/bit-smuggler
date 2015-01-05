{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BitSmuggler.TorrentFile (
    hashPieces
  , computeInfoHash
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

{-

functions for working with torrented files

-}

-- compute the hashes for the pieces of all the files of a torrent
hashPieces :: [FilePath] -> Int -> IO [ByteString]
hashPieces files pieceSize
  = runResourceT $ (P.foldl (>>) (return ()) $ P.map sourceFile files)
                    =$ hashEvery pieceSize $$ consume

hashEvery pieceSize = forever $ do
  piece <- fmap BSL.toStrict $ DCB.take pieceSize
  DC.yield $ SHA1.hash piece

computeInfoHash :: Torrent -> ByteString
computeInfoHash
  = SHA1.hash . BSL.toStrict . bPack
    . (\(BDict d) -> fromJust $ M.lookup "info" d) . serializeTorrent

