{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

Mini @bounded-enum@ framework inside @relude@.

@since 0.1.0
-}

module Relude.Extra.Enum
    ( next
    , prev
    , safeToEnum
    ) where

import Relude


{- | Like 'succ', but doesn't fail on 'maxBound'. Instead it returns 'minBound'.

>>> next False
True
>>> next True
False
>>> succ True
*** Exception: Prelude.Enum.Bool.succ: bad argument

@since 0.1.0
-}
next :: (Eq a, Bounded a, Enum a) => a -> a
next e
    | e == maxBound = minBound
    | otherwise     = succ e
{-# INLINE next #-}

{- | Like 'pred', but doesn't fail on 'minBound'. Instead it returns 'maxBound'.

>>> prev True
False
>>> prev False
True
>>> pred False
*** Exception: Prelude.Enum.Bool.pred: bad argument

@since 0.6.0.0
-}
prev :: (Eq a, Bounded a, Enum a) => a -> a
prev e
    | e == minBound = maxBound
    | otherwise     = pred e
{-# INLINE prev #-}

{- | Returns 'Nothing' if given 'Int' outside range.

>>> safeToEnum @Bool 0
Just False
>>> safeToEnum @Bool 1
Just True
>>> safeToEnum @Bool 2
Nothing
>>> safeToEnum @Bool (-1)
Nothing

@since 0.1.0
-}
safeToEnum :: forall a . (Bounded a, Enum a) => Int -> Maybe a
safeToEnum i = guard (fromEnum @a minBound <= i && i <= fromEnum @a maxBound) $> toEnum i
{-# INLINE safeToEnum #-}
