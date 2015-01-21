{-# LANGUAGE OverloadedStrings #-}
module TorrentFileSpec (main, spec) where

import Test.Hspec
import Prelude as P
import Control.Monad.IO.Class
import Data.Map.Lazy as Map
import System.IO.Temp
import Data.Conduit
import Data.Conduit.Binary as DC
import Data.Serialize
import Data.Conduit.Cereal
import Control.Monad.Trans.Resource
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
import Network.BitSmuggler.BitTorrentParser as BT


main :: IO ()
main = hspec spec

tempDir = "testDir"

dataFileSmall = "test-data/randFileSmall"
dataFileSmallTFile = "test-data/randFileSmall.torrent"


spec :: Spec
spec = do
  describe "makePartial" $ do
    it "zeroes out the missing pieces of a torrentable file" $ do
      return ()
 
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

  describe "makeBlockLoader" $ do
    it "loads the same blocks as the ones streamed in a bittorrent session" $ do
      let torrentStream = "test-data/seedClientCapture"
      (Right torrentFile) <- fmap readTorrent $ BSL.readFile dataFileSmallTFile
      blockLoad <- makeBlockLoader (tInfo torrentFile) dataFileSmall
      chunks <- runResourceT $ sourceFile torrentStream 
              =$ conduitGet (get :: Get StreamChunk) $$ DCL.consume
      let justPieces = P.filter isPiece $ P.drop 0 chunks 
      (P.length justPieces >= 1) `shouldBe` True
      sames <- forM justPieces $ \(MsgChunk _ p) -> do
        return $ blockLoad (fromIntegral $ BT.index p, Block {blockOffset = begin p,
                        blockSize  = BS.length $ block p}) == (block p)
      P.length (P.filter (P.id) sames) `shouldBe` P.length justPieces
      return ()
        
  return ()


