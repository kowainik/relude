{-
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
SPDX-License-Identifier: MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module Main (main) where

import Relude

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest
        $ "-XInstanceSigs"
        : "-XNoImplicitPrelude"
        : "-XOverloadedStrings"
        : "-XScopedTypeVariables"
        : "-XTypeApplications"
        : sourceFiles
