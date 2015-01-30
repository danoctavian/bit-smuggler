 {-# LANGUAGE OverloadedStrings #-}

module Network.BitSmuggler.TorrentClientProc (
    uTorrentProc
  , PreBootSetting (..) 
  , TorrentProc (..)
) where

import Prelude as P
import Data.Word
import Shelly as Sh
import Data.Serialize
import Data.List as DL
import Data.String
import Control.Monad
import System.IO

import Network.BitSmuggler.Utils

data PreBootSetting = CmdPort Word16 | BindPort Word16

data TorrentProc = TorrentProc {
    cleanState :: IO ()
  , start :: IO ()
  , setSettings :: [PreBootSetting] -> IO ()
  , getFilePath :: P.FilePath -> P.FilePath
}


uTorrentProc root = do
  path <- fmap fromString $ absolutePath root  
  return $ TorrentProc {
    cleanState = cleanUTorrentState path
  , start = (shelly $ chdir path $ Sh.run (path </> ("utserver" :: Sh.FilePath)) [])
            >> return ()

  , setSettings = \ss -> writeFile
                         (pathToString $ path </> ("utserver.conf" :: Sh.FilePath))
                         (P.concat $ intersperse "\n" $ P.map utConf ss)
  , getFilePath = \file ->pathToString $ path </> file
}

utConfVar name val = name ++ ": " ++ val
utConf (CmdPort p) = utConfVar "ut_webui_port" (show p)
utConf (BindPort p) = utConfVar "BIND_PORT" (show p)

uTorrentStateFiles :: [String]
uTorrentStateFiles = ["dht.dat", "resume.dat", "rss.dat", "settings.dat", "dht_feed.dat"]

cleanUTorrentState :: Sh.FilePath -> IO ()
cleanUTorrentState torrentPath = do
  forM (P.map (torrentPath </> )
    $ uTorrentStateFiles P.++ (P.map (P.++ ".old") uTorrentStateFiles))
    $ \file -> do
      r <- try' $ shelly $  rm file
      return ()
  return ()


-- this is so stupid I don't even..
pathToString ::Sh.FilePath -> String
pathToString fp = read . P.drop (P.length $ ("FilePath " :: String)) .  show $ fp

