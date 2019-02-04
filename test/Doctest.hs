{-
Copyright:  (c) 2016 Stephen Diehl
            (c) 20016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest $ "-XNoImplicitPrelude" : sourceFiles
