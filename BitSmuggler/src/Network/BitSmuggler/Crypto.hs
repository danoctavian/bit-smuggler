module Network.BitSmuggler.Crypto where

import Prelude as P
import Crypto.Elligator
import Crypto.Curve25519
import Crypto.Cipher.AES
import Data.ByteString
import Crypto.Cipher.Types
{-

 === crypto protocol ===

Client                                                                    Server

knows Pkserver

(Pk-client, RandStringRepr) = elligatorInv(Sk-client)

-> RandStringRepr ->

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

data EncryptedMessage = EncryptedMessage (IV AES256) AuthTag 

-- TODO: implement the following


-- derive client message to server (its pub key) and the encrypt/decrypt functions

-- derive server encrypt/decrypt from client message and server private key

-- encrypt functions require 32 bytes of entropy to encrypt




