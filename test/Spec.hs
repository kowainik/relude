module Main (main) where

import Relude

import Hedgehog (checkParallel)
import System.IO (hSetEncoding, utf8)
import Test.Relude.Property (hedgehogTestList)


main :: IO ()
main = do
  -- fix terminal encoding
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  mapM checkParallel hedgehogTestList >>= \p ->
      if and p
      then exitSuccess
      else exitFailure
