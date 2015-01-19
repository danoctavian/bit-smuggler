{-# LANGUAGE OverloadedStrings #-}
module CryptoSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude as P

import Data.ByteString as BS
import Data.ByteString.Arbitrary
import Data.Serialize as DS
import Data.Byteable
import Crypto.Curve25519
import Crypto.Random
import Crypto.Random.AESCtr as AESCtr

import Network.BitSmuggler.Utils
import Network.BitSmuggler.Crypto as Crypto


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "decrypts" $ do
    it "encrypted message matches decrypted" $ 
      property $ testEncryptDecrypt
  return ()


testEncryptDecrypt :: ArbByteString -> ArbByteString -> Bool
testEncryptDecrypt abs entropySeed
  = decrypt serverCrypto (encrypt clientCrypto iv message) == Just message
    && decrypt clientCrypto (encrypt serverCrypto iv message) == Just message
    where
      message = fromABS abs
      rng :: AESRNG
      rng =  cprgCreate $ createTestEntropyPool $ fromABS entropySeed
      -- iv
      (ivBytes, next) = cprgGenerate Crypto.ivLen rng
      (skBytes, next2) = cprgGenerate Crypto.keySize next
      iv  = fromRight $ DS.decode ivBytes :: Entropy
      serverSkWord = (fromRight $ DS.decode skBytes :: Key)
      serverSk = fromBytes $ toBytes serverSkWord
      serverPk = derivePublicKey serverSk
      serverPkWord = (fromRight $ DS.decode (toBytes serverPk) :: Key)
      (clientCrypto, repr) = makeClientEncryption serverPkWord rng
      serverCrypto = makeServerEncryption serverSkWord repr 
