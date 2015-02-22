{-# LANGUAGE OverloadedStrings #-}
module IntegrationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Prelude as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bit-smuggler" $ do
    it "proxies data between 1 client and 1 server" $ do
      P.putStrLn "wtf"

  return ()


