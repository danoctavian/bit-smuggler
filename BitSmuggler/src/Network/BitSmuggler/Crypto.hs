{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 

module Network.BitSmuggler.Crypto where

import Prelude as P
import Crypto.Elligator
import Crypto.Curve25519
import Crypto.Cipher.AES
import Data.ByteString as BS
import Crypto.Cipher.Types hiding (Key)
import Data.LargeWord
import Data.Byteable
import Data.Serialize as DS
import Control.Applicative
import Data.Maybe
import Data.Serialize.Get
import Control.Monad
import Crypto.Random
import Data.List as L

import Crypto.Random
import Crypto.Random.AESCtr as AESCtr

import Network.BitSmuggler.Utils
-- import Data.Serialize
{-

 === crypto protocol ===

Client                                                                    Server

knows Pkserver

(Pk-client, RandStringRepr) = elligatorInv(Sk-client)

-> RandStringRepr ++ handshakeMessage ->

                                                     Pk-client = elligator(RandStringRepr)

                              ===Shared secret derivation===

SharedSecret = Curve25519(Sk-client, Pk-server) 
                                            SharedSecret = Curve25519(Sk-server, Pk-client)

                              === Start message exchange ===

encryption mode : AES-GCM
message format: IV ++ authTag ++ cyphertext

IV - 32 bytes randomly generated for each message and appended before cyphertext
authTag - used by AES-GCM mode to verify the authenticity of the data

AAD (additional authenticated data) - is not used at the moment - can be used to check if tampering is done on the rest of the bittorrent message

-} 

data HandshakeMessage
  = HandshakeMessage {clientPkRepr :: ByteString, hsPayload :: ByteString}

data EncryptedMessage = EncryptedMessage ByteString AuthTag ByteString

type Message = ByteString

-- encrypt functions require 128 random bits
type Encrypt = Word128 -> Message -> ByteString 
type Decrypt = ByteString -> Maybe Message
data CryptoOps = CryptoOps {encrypt :: Encrypt, decrypt :: Decrypt}

type Key = Word256

keySize = 32
ivLen = 16
authTagLen = 16
msgHeaderLen :: Int
msgHeaderLen = ivLen + authTagLen
-- TODO: implement the following


-- derive client message to server (its pub key) and the encrypt/decrypt functions
-- the runtime of this function does not have an upper bound - 
-- keys need to be tried until 1 is found that satisfies elligator requirements
makeClientEncryption ::  CPRG g => Key -> g -> CryptoOps
makeClientEncryption serverPkWord gen
  = makeCryptoOps privKey (pubFromWord256 serverPkWord)
    where
      (privKey, (pubKey, repr)) = fromJust $ P.foldl mplus Nothing
           $ P.map (\rands -> let key = fromBytes rands in fmap (key,) $ elligatorInv key)
           $ L.unfoldr (\g -> Just $ cprgGenerate keySize g) gen
      

-- derive server encrypt/decrypt and the handshake payload
-- from client handshake message and server private key
tryReadHandshake :: Key -> ByteString -> Maybe (CryptoOps, Message)
tryReadHandshake skWord clientMessage = do
  hs <- eitherToMaybe (DS.decode clientMessage :: Either String HandshakeMessage)
  let cryptoOps = makeServerEncryption skWord (clientPkRepr hs)
  decrypted <- decrypt cryptoOps $ hsPayload hs
  return (cryptoOps, decrypted)

makeServerEncryption skWord clientPkRepr
  = makeCryptoOps (fromBytes $ toBytes skWord) $ elligator clientPkRepr

makeCryptoOps ownSecretKey otherPubKey
  = CryptoOps {
    encrypt = \iv msg -> let (cipher, authTag) = encryptGCM aes (toBytes iv) "" msg in
                            DS.encode $ EncryptedMessage (toBytes iv) authTag cipher
    , decrypt = \msg -> do
      (EncryptedMessage iv authTag cipher) <- (eitherToMaybe $ decode msg)
      let (plaintext, decryptedAuthTag) = decryptGCM aes iv "" cipher
      when (decryptedAuthTag /= authTag) $ fail "authentication tag doesn't match"
      return plaintext
  }
    where
      aes = initAES $ toBytes $ diffieHellman ownSecretKey otherPubKey


-- the crypto pseudo random number generator of choice for this app
makeCPRG :: IO AESRNG
makeCPRG = AESCtr.make <$> createEntropyPool

-- serialization of the messages is only formed out of cyphertext
-- or random string representations of cryptographic keys
-- thus leaving no fingerprintable pattern.
      
instance Serialize EncryptedMessage where
  put (EncryptedMessage iv authTag cypher)
    = putByteString iv >> putByteString (toBytes authTag) >> putByteString cypher
  get = EncryptedMessage
         <$> (getBytes ivLen) <*> (fmap AuthTag $ getBytes authTagLen) <*> getRemaining


instance Serialize HandshakeMessage where
  put (HandshakeMessage repr payload) = putByteString repr >> putByteString payload
  get = HandshakeMessage <$> getBytes keySize  <*> getRemaining
