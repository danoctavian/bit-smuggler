{-# LANGUAGE OverloadedStrings #-}
module CryptoSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude as P
import Data.Maybe

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
  describe "handshake" $ do
    it "handshake created by client is understandable by server" $ 
      property $ serverDecryptsHandshake

  describe "encryption-decryption" $ do
    it "encrypted message matches decrypted" $ 
      property $ encryptedMatchesDecrypted 
  return ()

serverDecryptsHandshake :: ArbByteString -> Bool
serverDecryptsHandshake abs = isJust readHandshake && (P.snd $ fromJust readHandshake) == message
  where
    message = fromABS abs
    readHandshake = tryReadHandshake serverSkWord handshakeCipher
    handshakeCipher = encryptHandshake (clientCrypto, repr) iv message
    ((clientCrypto, repr), (_, serverSkWord), iv) = cryptoFromSeed "le fixed seed" -- $ fromABS entropySeed


encryptedMatchesDecrypted :: ArbByteString -> Bool
encryptedMatchesDecrypted abs
  = decrypt serverCrypto (encrypt clientCrypto iv message) == Just message
    && decrypt clientCrypto (encrypt serverCrypto iv message) == Just message
    where
      message = fromABS abs
      ((clientCrypto, _), (serverCrypto, _), iv) = cryptoFromSeed "FIXED" -- $ fromABS entropySeed
      

-- sets up the encrypt/decrypt functions and vals to test 1 encrypt-decrypt
cryptoFromSeed entropySeed = ((clientCrypto, repr), (serverCrypto, serverSkWord), iv)
  where
    rng :: AESRNG
    rng =  cprgCreate $ createTestEntropyPool entropySeed
    (ivBytes, next) = cprgGenerate Crypto.ivLen rng
    (skBytes, next2) = cprgGenerate Crypto.keySize next
    iv  = fromRight $ DS.decode ivBytes :: Entropy
    serverSkWord = (fromRight $ DS.decode skBytes :: Key)
    serverSk = fromBytes $ toBytes serverSkWord
    serverPk = derivePublicKey serverSk
    serverPkWord = (fromRight $ DS.decode (toBytes serverPk) :: Key)
    (clientCrypto, repr) = makeClientEncryption serverPkWord rng
    serverCrypto = makeServerEncryption serverSkWord repr 


