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

{-
cache of files stored in a directory
-}

data FileCache k = FileCache {
    put :: Ord k => k -> (Source IO ByteString) -> IO FilePath
  , lookup :: Ord k => k -> IO (Maybe FilePath)
  , close :: IO ()
}

mapFile = "mapFile.cache"

load :: (Ord a, Serialize a) => FilePath -> IO (FileCache a)
load root = do
  (Right files) <- fmap decode $ BS.readFile mapFile
  tvar <- newTVarIO files
  return $ FileCache {
      lookup = \k -> (atomically $ readTVar tvar) >>= (return . Map.lookup k)
    , put  = \k source -> do
        (path, handle) <- openTempFile root "file.data" 
        source $$ sinkHandle handle
        hClose handle
        atomically $ modifyTVar tvar (Map.insert k path)
        return path
    , close = (atomically $ readTVar tvar) >>= BS.writeFile mapFile . encode
  } 


