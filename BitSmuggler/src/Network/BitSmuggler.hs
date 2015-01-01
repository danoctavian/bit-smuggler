module Network.BitSmuggler where

import Network.BitSmuggler.Crypto (Key)

{-



-}


data ClientConfig = ClientConfig

-- server
data ServerConfig = ServerConfig Key 

listen :: Key -> 
