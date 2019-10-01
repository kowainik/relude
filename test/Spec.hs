{-
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Main where

import Relude

import Test.Relude.Property (hedgehogTestList)

main :: IO ()
main = sequence_ hedgehogTestList
  
