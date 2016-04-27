{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Base (
  module X,
  ($!),
) where

-- Glorious Glashgow Haskell Compiler
#if defined(__GLASGOW_HASKELL__) && ( __GLASGOW_HASKELL__ >= 600 )

-- Base GHC types
import GHC.Num as X
import GHC.Enum as X
import GHC.Real as X
import GHC.Float as X
import GHC.Err as X (
    undefined
  , error
  )
import GHC.Show as X (
    Show(..)
  )
import GHC.Exts as X (
    Constraint
  , Ptr
  , FunPtr
  )
import GHC.Base as X (
    (++)
  , seq
  , asTypeOf
  , ord
  , maxInt
  , minInt
  )
import System.IO as X (
    print
  , putStr
  , putStrLn
  )

import GHC.Types as X (
    Bool
  , Char
  , Int
  , Word
  , Ordering
  , IO
#if ( __GLASGOW_HASKELL__ >= 710 )
  , Coercible
#endif
  )

#if ( __GLASGOW_HASKELL__ >= 800 )
import GHC.OverloadedLabels as X (
    IsLabel(..)
  )

{-
import GHC.Records as X (
    HasField(..)
  )
-}

import Data.Kind as X (
    type (*)
  , type Type
  )
#endif

infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx

#endif


-- Simple Haskell Compiler
#if defined(__SHC_HASKELL__)

import SHC.Prim as X
import SHC.Types as X
import SHC.Classes as X

#endif
