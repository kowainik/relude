{-
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

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

  mapM checkParallel hedgehogTestList >>= \p -> if and p then exitSuccess else exitFailure
