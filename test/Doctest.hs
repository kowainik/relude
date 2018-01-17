{-# LANGUAGE CPP #-}

module Main (main) where

#if MIN_VERSION_base(4,10,1)

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest $ "-XNoImplicitPrelude" : sourceFiles

#else

main :: IO ()
main = return ()

#endif
