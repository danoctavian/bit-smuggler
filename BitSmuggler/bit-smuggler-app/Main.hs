module Main where

import Prelude as P
import Data.Torrent
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC

import Data.ByteString.Lazy as BSL
import Control.Applicative
import System.Environment

import Network.BitSmuggler.Server
import Network.BitSmuggler.Client
import Network.BitSmuggler.TorrentFile
import Network.BitSmuggler.Utils
import Network.BitSmuggler.Common
import Network.BitSmuggler.DemoSetup


-- currently tiny commandline app to run some manual tests
main = do
  P.putStrLn "0 server, 1 client, 2 make cache, 3 socket ping"
  args <- getArgs  
  case read (P.head args) :: Int of
    0 -> runRealDemoServer
    1 -> runRealDemoClient
    2 -> initCache $ P.tail args
    3 -> bittorrentPing $ P.tail args
  return ()

initCache a = setupFileCache (a !! 0) (a !! 1) (a !! 2)

bittorrentPing a = socketPing (BSC.pack $ a !! 0) ( read (a !! 1) :: Int)

-- sends a bittorrent handshake message to see how a bittorrent clien
-- responds to it




