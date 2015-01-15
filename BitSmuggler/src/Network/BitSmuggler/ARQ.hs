module Network.BitSmuggler.ARQ where

import Foreign.Storable
import Data.Word
import Data.Default
import Data.ByteString as BS

import Data.Conduit as DC
import Data.Conduit.List as DC

{-
 == Automatic repeat request ARQ protocol ==
  
  protocol to deal with
    * disconnects in the bittorrent client connection
    * tcp packet tampering by an adversary
-}


type AckNum = Word32
type SeqNum = Word32

headerLen = sizeOf (def :: AckNum) + sizeOf (def :: SeqNum)


data ARQ = ARQ {
    sendARQ :: ARQPipe (Maybe ByteString)
  , recvARQ :: ARQPipe ByteString
}

type ARQPipe a = Conduit a IO a

noARQ :: ARQ 
noARQ = ARQ (DC.map id) (DC.map id)
