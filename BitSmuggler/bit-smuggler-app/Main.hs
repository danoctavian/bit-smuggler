module Main where

import Prelude as P
import Data.Torrent
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Control.Applicative

import Network.BitSmuggler.Server
import Network.BitSmuggler.Client
import Network.BitSmuggler.TorrentFile
import Network.BitSmuggler.Utils
import Network.BitSmuggler.Common


main = do
  P.putStrLn "running main"
  tFile <- (fromRight . readTorrent) <$> BSL.readFile "../demo/contactFile/testFile.torrent"
  let info = tInfo tFile
  let pieceCount = fromIntegral $ (tLength info) `div` (tPieceLength info)
  makeRandPartial  (fromIntegral $ tPieceLength info) pieceCount "../demo/contactFile/testFile.txt" "./partial" 
  return ()
  
 
