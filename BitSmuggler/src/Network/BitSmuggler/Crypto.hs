{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TupleSections #-} 

module Network.BitSmuggler.Crypto where

import Prelude as P
import Crypto.Elligator
import Crypto.Curve25519
import Crypto.Cipher.AES
import Data.ByteString as BS
import Crypto.Cipher.Types
import Data.LargeWord
import Data.Byteable
import Data.Serialize
import Control.Applicative
import Data.Maybe
import Data.Serialize.Get
import Control.Monad
import Crypto.Random
import Data.List as L

import Network.BitSmuggler.Utils
-- import Data.Serialize
{-

 === crypto protocol ===

Client                                                                    Server

knows Pkserver

(Pk-client, RandStringRepr) = elligatorInv(Sk-client)

-> RandStringRepr ++ handshakeMessage ->

                                                     Pk-client = elligator(RandStringRepr)

                              ===Diffie Hellman===

SharedSecret = DH(Sk-client, Pk-server)              SharedSecret = DH(Sk-server, Pk-client)

                              === Start message exchange ===

encryption mode : AES-GCM
message format: IV ++ authTag ++ cyphertext

IV - 32 bytes randomly generated for each message and appended before cyphertext
authTag - used by AES-GCM mode to verify the authenticity of the data

AAD (additional authenticated data) - is not used at the moment - can be used to check if tampering is done on the rest of the bittorrent message

-} 

data EncryptedMessage = EncryptedMessage ByteString AuthTag ByteString

type Message = ByteString

-- encrypt functions require 128 random bits
type Encrypt = Word128 -> Message -> ByteString 
type Decrypt = ByteString -> Maybe Message
data CryptoOps = CryptoOps {encrypt :: Encrypt, decrypt :: Decrypt}

keySize = 32
ivLen = 16
authTagLen = 16
msgHeaderLen = ivLen + authTagLen
-- TODO: implement the following


-- derive client message to server (its pub key) and the encrypt/decrypt functions

makeClientEncryption ::  CPRG g => Word256 -> g -> CryptoOps
makeClientEncryption serverPkWord gen
  = makeCryptoOps privKey (pubFromWord256 serverPkWord)
    where
      (privKey, (pubKey, repr)) = fromJust $ P.foldl mplus Nothing
           $ P.map (\rands -> let key = fromBytes rands in fmap (key,) $ elligatorInv key)
           $ L.unfoldr (\g -> Just $ cprgGenerate keySize g) gen
      

-- derive server encrypt/decrypt from client message and server private key
makeServerEncryption :: Word256 -> ByteString -> Maybe CryptoOps
makeServerEncryption skWord clientMessage
  = if (BS.length clientMessage >= fstMessageLen && (not $ isNothing decrypted)
        && (clientPkRepr == fromJust decrypted))
      then Just cryptoOps
      else Nothing
    where
      fstMessageLen = keySize + msgHeaderLen + keySize
      sk = fromBytes $ toBytes skWord
      (clientPkRepr, handshakeMsg) = BS.splitAt keySize $ BS.take fstMessageLen clientMessage
      cryptoOps = makeCryptoOps sk (elligator clientPkRepr) 
      decrypted = decrypt cryptoOps handshakeMsg
            
makeCryptoOps ownSecretKey otherPubKey
  = CryptoOps {
    encrypt = \iv msg -> let (cipher, authTag) = encryptGCM aes (toBytes iv) "" msg in
                            encode $ EncryptedMessage (toBytes iv) authTag cipher
    , decrypt = \msg -> do
      (EncryptedMessage iv authTag cipher) <- (eitherToMaybe $ decode msg)
      let (plaintext, decryptedAuthTag) = decryptGCM aes iv "" cipher
      when (decryptedAuthTag /= authTag) $ fail "not the same"
      return plaintext
  }
    where
      aes = initAES $ toBytes $ diffieHellman ownSecretKey otherPubKey
      
instance Serialize EncryptedMessage where
  put (EncryptedMessage iv authTag cypher)
    = putByteString iv >> putByteString (toBytes authTag) >> putByteString cypher
  get = EncryptedMessage
         <$> (getBytes ivLen) <*> (fmap AuthTag $ getBytes authTagLen) <*> getRemaining
