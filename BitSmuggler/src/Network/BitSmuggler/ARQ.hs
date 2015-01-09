module Network.BitSmuggler.ARQ where

import Foreign.Storable
import Data.Word
import Data.Default
{-
 == Automatic repeat request ARQ protocol ==
  
  protocol to deal with
    * disconnects in the bittorrent client connection
    * tcp packet tampering by an adversary
-}


type AckNum = Word32
type SeqNum = Word32

headerLen = sizeOf (def :: AckNum) + sizeOf (def :: SeqNum)
