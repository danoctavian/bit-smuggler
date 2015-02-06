{-# LANGUAGE  RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BitSmuggler.TorrentFile (
    hashPieces
  , computeInfoHash
  , textToInfoHash
  , makeBlockLoader
  , makePartial
  , BT.infoHashToString
  , LoadBlock
  , BlockLoader (..)
) where

import Crypto.Hash.SHA1 as SHA1
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC

import qualified Data.ByteString.Lazy as BSL

import Data.Serialize as DS
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
import System.IO

import Network.BitSmuggler.BitTorrentParser as BT
import Network.BitSmuggler.Utils

import qualified Network.BitTorrent.Types as BT

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

{-
  constructs a partial torrent file from a full one
  (takes out pieces by zeroing out that file region)
-}
makePartial pieceSize src dest filter = 
  runResourceT $ sourceFile src =$ filterEvery pieceSize filter 0 $$ sinkFile dest

filterEvery pieceSize filter ix = do
  piece <- fmap BSL.toStrict $ DCB.take pieceSize
  when (BS.length piece > 0) $ do
    DC.yield $ if filter ix piece then piece else BS.replicate (BS.length piece) 0
    filterEvery pieceSize filter (ix + 1)

textToInfoHash :: Text -> Maybe InfoHash
textToInfoHash t =  BT.textToInfoHash t >>= (eitherToMaybe . DS.decode)

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

type LoadBlock = (Int, BT.Block) -> IO ByteString
data BlockLoader = BlockLoader {loadBlock :: LoadBlock, closeLoader :: IO ()}

makeBlockLoader (SingleFile {..}) filePath = do
  -- TODO: figure out why memory mapping doesn't work
  -- file <- mmapFileByteStringLazy filePath Nothing
  -- it seems to just load the wrong things
  -- lazy loading the whole file isn't an option- it blows up the memory
  -- so now it's done with seek and get
  handle <- openFile filePath ReadMode

  return $ BlockLoader {
      loadBlock = \pos -> do 
        let (start, len) =  blockPos (fromIntegral tPieceLength) pos
        hSeek handle AbsoluteSeek start
        hGet handle len 
    , closeLoader = hClose handle 
  }

