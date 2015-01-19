{-# LANGUAGE  RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BitSmuggler.TorrentFile (
    hashPieces
  , computeInfoHash
  , textToInfoHash
  , makeBlockLoader
  , makePartial
) where

import Crypto.Hash.SHA1 as SHA1
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Torrent
import Data.Conduit.Binary as DCB
import Data.Conduit as DC
import Data.Conduit.List
import Control.Monad
import Control.Exception
import Prelude as P
import Control.Monad.Trans.Resource
import Data.Torrent
import Data.Map as M
import Data.BEncode
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding as T
import System.IO.MMap
import Data.ByteString.Base16 as Base16

import Network.BitSmuggler.BitTorrentParser
import Network.BitSmuggler.Utils

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


{-
  constructs a partial torrent file from a full one
  (takes out pieces by zeroing out that file region)
-}
makePartial pieceSize src dest filter = 
  runResourceT $ sourceFile src =$ filterEvery pieceSize filter 0 $$ sinkFile dest

filterEvery pieceSize filter ix = do
  piece <- fmap BSL.toStrict $ DCB.take pieceSize
  DC.yield $ if filter ix piece then piece else BS.replicate (BS.length piece) 0
  filterEvery pieceSize filter (ix + 1)
 

{-

== File fixing == 

File fixing means correcting the payload of a bittorrent block
sent in a piece message to the right value (if it has been tampered with
by BitSmuggler) before it reaches the real bittorrent client.

This is so the client doesn't misbehave when seeing a bunch of hash 
failures. if a way is found to tell the client to ignore those hash fails
this technique is no longer necessary. 

The only reason you might want to still use it would be for undetectability
purposes. By using it you would make sure that other peers joining the swarm
will be served correct pieces by the bittorrent clients so they wouldn't
see a bunch of hash fails themselves.

-- REQUIREMENT: FILE DATA MUST BE AVAILABLE  --

obviously the requirement is that the file data exists locally so the
correction can be done. How to get it is a separate issue.

-}

blockPos pieceLen (index, block)
  = (pieceLen * (fromIntegral index)
      + (fromIntegral $ blockOffset block), fromIntegral $ blockSize block)

makeBlockLoader (SingleFile {..}) filePath = do
  -- TODO: figure out why memory mapping doesn't work
  -- lazy loading the whole file isn't an option.
  -- mmapFileByteStringLazy filePath Nothing
  file <- BSL.readFile filePath
  return $ \pos -> -- this function is "pure" but it does lazy i/o
    let (start, len) =  blockPos (fromIntegral tPieceLength) pos
    in BSL.toStrict $ BSL.take len $ BSL.drop start file
