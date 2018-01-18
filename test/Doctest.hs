{-# LANGUAGE CPP #-}

module Main (main) where

#if defined(mingw32_HOST_OS) || __GLASGOW_HASKELL__ < 802

main :: IO ()
main = return ()

#else

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest $ "-XNoImplicitPrelude" : sourceFiles

#endif
