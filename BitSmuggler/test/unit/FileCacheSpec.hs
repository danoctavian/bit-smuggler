{-# LANGUAGE OverloadedStrings #-}
module FileCacheSpec (main, spec) where

import Test.Hspec
import Prelude as P
import Control.Monad.IO.Class
import Data.Map.Lazy as Map
import System.IO.Temp
import Data.Conduit
import qualified Data.Conduit.List as DCL 
-- import Data.Conduit.Binary
import System.IO
import Control.Monad.IO.Class
import Data.Serialize as DS
import Data.ByteString as BS
import Control.Monad 
import System.FilePath

import Network.BitSmuggler.FileCache as FC

main :: IO ()
main = hspec spec

tempDir = "testDir"

store cache items = forM items (\(k, c) -> FC.put cache k (DCL.sourceList [c]))

checkContains cache items
  = forM items $ \((k, c), destination) -> do
      path <- FC.lookup cache k     
      path `shouldBe` (Just destination)
      storedContent <- BS.readFile destination
      storedContent `shouldBe` c
     

spec :: Spec
spec = do
  describe "load" $ do
    it "loads from empty unconfigured dir" $ do
      withSystemTempDirectory tempDir $ \root -> do
        cache <- (load root :: IO (FileCache String))
        nothin <- FC.lookup cache "nonexistant"
        nothin `shouldBe` Nothing
      return ()
  describe "put" $ do
    it "contains what was stored" $ do
      withSystemTempDirectory tempDir $ \root -> do
        cache <- (load root :: IO (FileCache String))
        let items = [("k", "bytestring"), ("k2", "bullshit")]
        destinations <- store cache items
        checkContains cache (P.zip items destinations)
      return ()
  describe "close" $ do
    it "loads correctly after closing" $ do
     withSystemTempDirectory tempDir $ \root -> do
       cache <- (load root :: IO (FileCache String))
       let items = [("k", "bytestring")]
       destinations <- store cache items
       close cache
       reloaded <- (load root :: IO (FileCache String))
       checkContains reloaded (P.zip items destinations)
     return ()
    
  return ()


