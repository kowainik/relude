{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Stable
Portability: Portable

This module contains reexports from "Data.List.NonEmpty" and safe functions to
work with list type in terms of 'NonEmpty'.

Note, that "Relude" reexports 'head', 'tail', 'init', 'last' from
"Data.List.NonEmpty" instead of the "Data.List", so these functions are safe to
use.

+------------+--------------+-----------------------+
|            |    @base@    |       @relude@        |
+============+==============+=======================+
| __'head'__ | @[a] -> a@   | @'NonEmpty' a -> a@   |
+------------+--------------+-----------------------+
| __'tail'__ | @[a] -> [a]@ | @'NonEmpty' a -> [a]@ |
+------------+--------------+-----------------------+
| __'last'__ | @[a] -> a@   | @'NonEmpty' a -> a@   |
+------------+--------------+-----------------------+
| __'init'__ | @[a] -> [a]@ | @'NonEmpty' a -> [a]@ |
+------------+--------------+-----------------------+

@relude@ also provides custom type error for better experience with transition
from lists to 'NonEmpty' with those functions.


Let's examine the behaviour of the @relude@ list functions comparing to the
corresponding @base@ one on the example of the 'head' function:

+-----------------+-----------------------------------------+
|                 |         'head'                          |
+=================+=========================================+
| __@base@__      | @[a] -> a@                              |
+-----------------+-----------------------------------------+
| __@relude@__    | @'NonEmpty' a -> a@                     |
+-----------------+-----------------------------------------+
| Example         | @> 'Data.List.head' [1..5]@             |
| with list       +-----------------------------------------+
| __@base@__      | @1@                                     |
+-----------------+-----------------------------------------+
| Example         | @> 'Data.List.head' []@                 |
| with empty list +-----------------------------------------+
| __@base@__      |@*** Exception: Prelude.head: empty list@|
+-----------------+-----------------------------------------+
| Example         |@> 'head' $ 1 :| [2..5]@                 |
| with 'NonEmpty' +-----------------------------------------+
| __@relude@__    | @1@                                     |
+-----------------+-----------------------------------------+
| Example         | @> 'viaNonEmpty' 'head' [1..5]@         |
| with list       +-----------------------------------------+
| __@relude@__    | @'Just' 1@                              |
+-----------------+-----------------------------------------+
| Example         |@> 'viaNonEmpty' 'head' []@              |
| with empty list +-----------------------------------------+
| __@relude@__    | @'Nothing'@                             |
+-----------------+-----------------------------------------+

@since 0.2.0
-}

module Relude.List.NonEmpty
    ( -- * Reexports from "DataList.NonEmpty"
      NonEmpty (..)
    , nonEmpty
    , head
    , tail
    , last
    , init

      -- * Combinators
    , viaNonEmpty
    , whenNotNull
    , whenNotNullM
    ) where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

import Relude.Applicative (Applicative, pass)
import Relude.Base (Constraint, Type)
import Relude.Function ((.))
import Relude.Functor (fmap)
import Relude.Monad (Maybe (..), Monad (..))

import qualified Data.List.NonEmpty as NE (head, init, last, tail)


-- $setup
-- >>> import Relude

{- | For safe work with lists using functions for 'NonEmpty'.

>>> viaNonEmpty head [1]
Just 1
>>> viaNonEmpty head []
Nothing

@since 0.1.0
-}
viaNonEmpty :: (NonEmpty a -> b) -> [a] -> Maybe b
viaNonEmpty f = fmap f . nonEmpty
{-# INLINE viaNonEmpty #-}

{- | Performs given action over 'NonEmpty' list if given list is non empty.

>>> whenNotNull [] $ \(b :| _) -> print (not b)
>>> whenNotNull [False,True] $ \(b :| _) -> print (not b)
True
-}
whenNotNull :: Applicative f => [a] -> (NonEmpty a -> f ()) -> f ()
whenNotNull []     _ = pass
whenNotNull (x:xs) f = f (x :| xs)
{-# INLINE whenNotNull #-}

-- | Monadic version of 'whenNotNull'.
whenNotNullM :: Monad m => m [a] -> (NonEmpty a -> m ()) -> m ()
whenNotNullM ml f = ml >>= \l -> whenNotNull l f
{-# INLINE whenNotNullM #-}


-- | For tracking usage of ordinary list with @head@-like functions.
type IsNonEmpty
    (f :: Type -> Type)  -- Container, e.g. NonEmpty or []
    (a :: Type)          -- Type of the element
    (res :: Type)        -- Type of the result of the work of the function
    (fun :: Symbol)      -- Function name
    = (f ~ NonEmpty, CheckNonEmpty f a res fun)

type family CheckNonEmpty
    (f :: Type -> Type)
    (a :: Type)
    (res :: Type)
    (fun :: Symbol)
    :: Constraint
  where
    CheckNonEmpty NonEmpty _ _ _ = ()
    CheckNonEmpty [] a res fun = TypeError
        ( 'Text "'" ':<>: 'Text fun ':<>: 'Text "' works with 'NonEmpty', not ordinary lists."
        ':$$: 'Text "Possible fix:"
        ':$$: 'Text "    Replace: [" ':<>: 'ShowType a ':<>: 'Text "]"
        ':$$: 'Text "    With:    NonEmpty " ':<>: 'ShowType a
        ':$$: 'Text ""
        ':$$: 'Text "However, you can use '" ':<>: 'Text fun ':<>: 'Text "' with the ordinary lists."
        ':$$: 'Text "Apply 'viaNonEmpty' function from relude:"
        ':$$: 'Text "    viaNonEmpty " ':<>: 'Text fun ':<>: 'Text " (yourList)"
        ':$$: 'Text "Note, that this will return 'Maybe " ':<>: 'ShowType res ':<>: 'Text "'"
        ':$$: 'Text "therefore it is a safe function unlike '" ':<>: 'Text fun ':<>: 'Text "' from the standard Prelude"
        )
    CheckNonEmpty t a _ fun = TypeError
        ( 'Text "'"
        ':<>: 'Text fun
        ':<>: 'Text "' works with 'NonEmpty "
        ':<>: 'ShowType a
        ':<>: 'Text "' lists"
        ':$$: 'Text "But given: "
        ':<>: 'ShowType t
        ':<>: 'Text " "
        ':<>: 'ShowType a
        )


{- | @O(1)@. Extracts the first element of a 'NonEmpty' list.

Actual type of this function is the following:

@
head :: 'NonEmpty' a -> a
@

but it was given a more complex type to provide friendlier compile time errors.

>>> head ('a' :| "bcde")
'a'
>>> head [0..5 :: Int]
...
... 'head' works with 'NonEmpty', not ordinary lists.
      Possible fix:
          Replace: [Int]
          With:    NonEmpty Int
...
      However, you can use 'head' with the ordinary lists.
      Apply 'viaNonEmpty' function from relude:
          viaNonEmpty head (yourList)
      Note, that this will return 'Maybe Int'
      therefore it is a safe function unlike 'head' from the standard Prelude
...
>>> head (Just 'a')
...
... 'head' works with 'NonEmpty Char' lists
      But given: Maybe Char
...
-}
head :: IsNonEmpty f a a "head" => f a -> a
head = NE.head
{-# INLINE head #-}

{- | @O(n)@. Return all the elements of a 'NonEmpty' list except the last one
element.

Actual type of this function is the following:

@
init :: 'NonEmpty' a -> [a]
@

but it was given a more complex type to provide friendlier compile time errors.

>>> init ('a' :| "bcde")
"abcd"
>>> init [0..5 :: Int]
...
... 'init' works with 'NonEmpty', not ordinary lists.
      Possible fix:
          Replace: [Int]
          With:    NonEmpty Int
...
      However, you can use 'init' with the ordinary lists.
      Apply 'viaNonEmpty' function from relude:
          viaNonEmpty init (yourList)
      Note, that this will return 'Maybe [Int]'
      therefore it is a safe function unlike 'init' from the standard Prelude
...
>>> init (Just 'a')
...
... 'init' works with 'NonEmpty Char' lists
      But given: Maybe Char
...
-}
init :: IsNonEmpty f a [a] "init" => f a -> [a]
init = NE.init
{-# INLINE init #-}

{- | @O(n)@. Extracts the last element of a 'NonEmpty' list.

Actual type of this function is the following:

@
last :: 'NonEmpty' a -> a
@

but it was given a more complex type to provide friendlier compile time errors.

>>> last ('a' :| "bcde")
'e'
>>> last [0..5 :: Int]
...
... 'last' works with 'NonEmpty', not ordinary lists.
      Possible fix:
          Replace: [Int]
          With:    NonEmpty Int
...
      However, you can use 'last' with the ordinary lists.
      Apply 'viaNonEmpty' function from relude:
          viaNonEmpty last (yourList)
      Note, that this will return 'Maybe Int'
      therefore it is a safe function unlike 'last' from the standard Prelude
...
>>> last (Just 'a')
...
... 'last' works with 'NonEmpty Char' lists
      But given: Maybe Char
...
-}
last :: IsNonEmpty f a a "last" => f a -> a
last = NE.last
{-# INLINE last #-}

{- | @O(1)@. Return all the elements of a 'NonEmpty' list after the head
element.

Actual type of this function is the following:

@
tail :: 'NonEmpty' a -> [a]
@

but it was given a more complex type to provide friendlier compile time errors.

>>> tail ('a' :| "bcde")
"bcde"
>>> tail [0..5 :: Int]
...
... 'tail' works with 'NonEmpty', not ordinary lists.
      Possible fix:
          Replace: [Int]
          With:    NonEmpty Int
...
      However, you can use 'tail' with the ordinary lists.
      Apply 'viaNonEmpty' function from relude:
          viaNonEmpty tail (yourList)
      Note, that this will return 'Maybe [Int]'
      therefore it is a safe function unlike 'tail' from the standard Prelude
...
>>> tail (Just 'a')
...
... 'tail' works with 'NonEmpty Char' lists
      But given: Maybe Char
...
-}
tail :: IsNonEmpty f a [a] "tail" => f a -> [a]
tail = NE.tail
{-# INLINE tail #-}
