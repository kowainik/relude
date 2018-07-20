module Main where

import Test.Tasty (defaultMain)

import Test.Relude.Property (hedgehogTestTree)

main :: IO ()
main = do
    defaultMain hedgehogTestTree
