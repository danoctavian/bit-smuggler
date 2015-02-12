module Network.BitSmuggler.StreamMultiplex where


import Data.Conduit as DC
import Data.Conduit.List as DC
import Data.Conduit.Cereal
import Network.TCP.Proxy.Server

import Network.BitSmuggler.Utils

{-
multiplexer for streams of bytestring

-}

type InitConn = RemoteAddr ->  (ConnData -> IO ()) -> IO ()

withClient :: ConnData -> (InitConn -> IO ()) -> IO ()
withClient sharedPipe handler = do
  

{-
  concurrently (sourceTQueue =$ conduitPut $$ (connSink sharedPipe))
               ((connSource sharedPipe =$ conduitGet))
-}
  return ()
  
  
