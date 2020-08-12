{-# LANGUAGE Safe #-}
{-# LANGUAGE BangPatterns #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

Contains utility functions for working with tuples.

@since 0.6.0.0
-}

module Relude.Extra.Foldable
    ( foldlSC
    , average
    ) where

import Relude


{- | A left-associative fold that's tail-recursive but can still short-circuit.
Returning a 'Left' short-circuits and immediately returns the value inside.
Returning a 'Right' continues the fold as usual with the value inside.

>>> foldlSC (\acc x -> if x == 0 then Left 0 else Right $! acc * x) 1 [1..6]
720
>>> foldlSC (\acc x -> if x == 0 then Left 0 else Right $! acc * x) 1 (0:error "Short-circuiting should keep this from happening")
0

@since 0.6.0.0
-}
foldlSC :: forall t b a. Foldable t => (b -> a -> Either b b) -> b -> t a -> b
foldlSC f = flip $ foldr go id
  where
    go :: a -> (b -> b) -> b -> b
    go x k z = case f z x of
        Left l  -> l
        Right r -> k r
{-# INLINE foldlSC #-}

{- | Compute average of 'Foldable'
-}
average :: (Foldable f, Fractional a) => f a -> Maybe a
average xs
    | null xs = Nothing
    | otherwise = Just
        . uncurry (/)
        . foldl' (\(!total, !count) x -> (total + x, count + 1)) (0,0)
        $ xs
{-# INLINE average #-}

