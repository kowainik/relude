module Main where

import Test.Tasty (defaultMain)

import Test.Universum.Property (hedgehogTestTree)

main :: IO ()
main = do
    defaultMain hedgehogTestTree
