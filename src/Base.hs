{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
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
import GHC.Show as X
import GHC.Exts as X (
    Constraint
  , Ptr
  , FunPtr
  , the
  )
import GHC.Base as X (
    (++)
  , seq
  , asTypeOf
  )
import System.IO as X (
    print
  , putStr
  , putStrLn
  )

#if ( __GLASGOW_HASKELL__ >= 800 )
import GHC.Types as X hiding (Any)
#else
import GHC.Types as X
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
