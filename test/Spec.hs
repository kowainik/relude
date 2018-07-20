{-
Copyright: (c) 2016 Stephen Diehl
           (c) 20016-2018 Serokell
           (c) 2018 Kowainik
License: MIT
-}

module Main where

import Test.Tasty (defaultMain)

import Test.Relude.Property (hedgehogTestTree)

main :: IO ()
main = do
    defaultMain hedgehogTestTree
