module Main where

import Prelude as P
import Data.Torrent
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC

import Data.ByteString.Lazy as BSL
import Control.Applicative
import System.Environment
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary

import Network.BitSmuggler.Server
import Network.BitSmuggler.Client
import Network.BitSmuggler.TorrentFile
import Network.BitSmuggler.Utils
import Network.BitSmuggler.Common
import Network.BitSmuggler.DemoSetup

-- currently tiny commandline app to run some manual tests
main = do
  P.putStrLn "0 server, 1 client, 2 make cache, 3 generate file"
  args <- getArgs  
  case read (P.head args) :: Int of
    0 -> return () -- runRealDemoServer
    1 -> return () --runRealDemoClient
    2 -> return () -- initCache $ P.tail args
      -- seed, size, filename
    3 -> genRandFile $ P.tail args
  return ()

--initCache a = setupFileCache (a !! 0) (a !! 1) (a !! 2)

-- sends a bittorrent handshake message to see how a bittorrent clien
-- responds to it

genRandFile a =
  runResourceT $ genRandBytes (read (a !! 0) ::Int) (read (a !! 1) :: Int) $$ sinkFile (a !! 2)

