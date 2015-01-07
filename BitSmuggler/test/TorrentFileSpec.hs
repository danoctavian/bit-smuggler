{-# LANGUAGE OverloadedStrings #-}
module TorrentFileSpec (main, spec) where

import Test.Hspec
import Prelude as P
import Control.Monad.IO.Class
import Data.Map.Lazy as Map
import System.IO.Temp
import Data.Conduit
import qualified Data.Conduit.List as DCL 
import System.IO
import Control.Monad.IO.Class
import Data.Serialize as DS
import Data.ByteString.Lazy as BSL
import Data.ByteString as BS
import Control.Monad 
import System.FilePath
import Data.Torrent
import Data.Maybe

import Network.BitSmuggler.TorrentFile as TF

main :: IO ()
main = hspec spec

tempDir = "testDir"

dataFileSmall = "test-data/randFileSmall"
dataFileSmallTFile = "test-data/randFileSmall.torrent"


spec :: Spec
spec = do
  describe "hashPieces" $ do
    it "matches piece hashes in model torrent file" $ do
      (Right torrentFile) <- fmap readTorrent $ BSL.readFile dataFileSmallTFile
      let tinfo = tInfo $ torrentFile
      hashes <- hashPieces [dataFileSmall] (fromIntegral $ tPieceLength tinfo)
      BSL.fromChunks hashes `shouldBe` (tPieces tinfo) 
      return ()
  describe "computeInfoHash" $ do
    it "matches expected value" $ do
      (Right torrentFile) <- fmap readTorrent $ BSL.readFile dataFileSmallTFile
      computeInfoHash torrentFile
        `shouldBe` (fromJust $textToInfoHash "b5724467037c1c4192049b84bba92ea2bdf3d445")
  return ()


