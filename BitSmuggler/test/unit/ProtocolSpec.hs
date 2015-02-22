{-# LANGUAGE OverloadedStrings #-}
module ProtocolSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.ByteString as BS
import Control.Monad.ST
import System.IO.Unsafe
import Data.ByteString.Arbitrary
import Prelude as P
import Network.BitSmuggler.Protocol as Proto
import Network.BitSmuggler.Crypto as Crypto

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pipes" $ do
    it "send pipe fused with the recv pipe is the id function (roughly :) )" $ do
      property $ sendFuseRecvIsId
    return ()
  describe "takeWhile" $ do
    it "takes while :)" $ do
      (runST (sourceList [Just "wtf"] $$ takeWhileSmth 2)) `shouldBe` (Just "wt")
      (runST (sourceList [Nothing, Just "wtf"] $$ takeWhileSmth 2)) `shouldBe` (Just "")
      (runST (sourceList [Just "omg", Just "wtf"] $$ takeWhileSmth 4)) `shouldBe` (Just "omgw")
      (runST (sourceList [Just "jeez", Just "man"] $$ (takeWhileSmth 7 >> takeWhileSmth 1)))
        `shouldBe` Nothing 
  return ()


testPacketSize = blockSize - Crypto.msgHeaderLen -- just crypto, no ARQ

sendFuseRecvIsId arbBs
  = unsafePerformIO (sourceList (P.map Just wireMsgs) =$ send =$= catMaybes =$= recv $$ DC.take (P.length wireMsgs))
    == wireMsgs
    where
      messages = P.map fromABS arbBs
      recv = recvPipe (DC.map P.id) fakeDecrypt
      send = sendPipe testPacketSize (DC.map P.id) fakeEncrypter
      wireMsgs :: [WireMessage ServerMessage]
      wireMsgs = P.map (Data . DataChunk) messages

-- fake crypto 
-- we're not testing that here so we're just mocking it

-- courtesy of random.org
fakeCryptoHeader = BS.replicate Crypto.msgHeaderLen 101

fakeDecrypt m = if (BS.take Crypto.msgHeaderLen m == fakeCryptoHeader)
                then Just $ BS.drop Crypto.msgHeaderLen m
                else Nothing

fakeEncrypter = Encrypter {runE = \bs -> (BS.concat [fakeCryptoHeader, bs], fakeEncrypter)}
