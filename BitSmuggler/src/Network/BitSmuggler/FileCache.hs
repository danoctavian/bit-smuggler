{-# LANGUAGE PackageImports, RankNTypes #-}

module Network.BitSmuggler.FileCache where

import Prelude as P hiding (lookup)
import Data.ByteString as BS
import Data.Serialize as DS hiding (put)
import qualified Data.Map.Lazy as Map
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import "temporary" System.IO.Temp
import Data.Conduit
import Data.Conduit.Binary
import System.IO
import Control.Monad.IO.Class
import Control.Monad
import System.FilePath
import System.Directory

{-
cache of files stored in a directory

WARNING: not thread-safe
-}

data FileCache k = FileCache {
    put :: Ord k => k -> (Source IO ByteString) -> IO FilePath
  , lookup :: Ord k => k -> IO (Maybe FilePath)
  , close :: IO ()
}

mapFile = "mapFile.cache"

load :: (Ord a, Serialize a) => FilePath -> IO (FileCache a)
load root = do
  let mapFilePath = root </> mapFile
  exists <- doesFileExist mapFilePath
  files <- if exists
           then fmap ((\(Right m) -> m) . decode) $ BS.readFile mapFilePath
           else return (Map.empty)
  tvar <- newTVarIO files
  return $ FileCache {
      lookup = \k -> (atomically $ readTVar tvar) >>= (return . Map.lookup k)
    , put  = \k source -> do
        map <- atomically $ readTVar tvar
        case Map.lookup k map of
          Just path -> return path -- don't store again
          Nothing -> do
            (path, handle) <- openTempFile root "file.data" 
            source $$ sinkHandle handle
            hClose handle
            atomically $ modifyTVar tvar (Map.insert k path)
            return path
    , close = (atomically $ readTVar tvar) >>= BS.writeFile mapFilePath . encode
  } 
