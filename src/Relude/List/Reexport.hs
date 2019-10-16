{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Trustworthy          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright:  (c) 2016 Stephen Diehl
            (c) 2016-2018 Serokell
            (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Reexports most of the "Data.List" and "Data.List.NonEmpty".

Note, that Relude reexports 'head', 'tail', 'init', 'last' from
"Data,List.NonEmpty" instead of the "Data.List", so these functions are safe to
use.

`relude` also provides custom type error for better experience with transition
from lists to 'NonEmpty' with those functions.
-}

module Relude.List.Reexport
       ( -- * List
         module Data.List
       , sortWith
         -- * NonEmpty List
       , NonEmpty (..)
       , nonEmpty
       , head
       , init
       , last
       , tail
       ) where

import Data.Kind (Type)
import Data.List (break, cycle, drop, dropWhile, filter, genericDrop, genericLength,
                  genericReplicate, genericSplitAt, genericTake, group, inits, intercalate,
                  intersperse, isPrefixOf, iterate, map, permutations, repeat, replicate, reverse,
                  scanl, scanr, sort, sortBy, sortOn, splitAt, subsequences, tails, take, takeWhile,
                  transpose, uncons, unfoldr, unzip, unzip3, zip, zip3, zipWith, (++))
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import GHC.Exts (Constraint, sortWith)
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

import qualified Data.List.NonEmpty as NE (head, init, last, tail)


-- $setup
-- >>> import Relude


-- | For tracking usage of ordinary list with @head@-like functions.
type IsNonEmpty
    (f :: Type -> Type)  -- Container, e.g. NonEmpty or []
    (a :: Type)          -- Type of the element
    (res :: Type)        -- Type of the result of the work of the function
    (fun :: Symbol)      -- Function name
    = (f ~ NonEmpty, CheckNonEmpty f a res fun)

type family CheckNonEmpty (f :: Type -> Type) (a :: Type) (res ::Type) (fun :: Symbol) :: Constraint where
    CheckNonEmpty NonEmpty _ _ _ = ()
    CheckNonEmpty [] a res fun = TypeError
        ( 'Text "'" ':<>: 'Text fun ':<>: 'Text "' is working with 'NonEmpty', not ordinary lists."
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
    CheckNonEmpty _ a _ fun = TypeError
        ( 'Text "'"
          ':<>: 'Text fun
          ':<>: 'Text "' is working with 'NonEmpty "
          ':<>: 'ShowType a
          ':<>: 'Text "' lists"
        )


{- | @O(1)@. Extracts the first element of a 'NonEmpty' list.

>>> head ('a' :| "bcde")
'a'
>>> head [0..5 :: Int]
...
... 'head' is working with 'NonEmpty', not ordinary lists.
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
... 'head' is working with 'NonEmpty Char' lists
...
-}
head :: IsNonEmpty f a a "head" => f a -> a
head = NE.head

{- | @O(n)@. Return all the elements of a 'NonEmpty' list except the last one
element.

>>> init ('a' :| "bcde")
"abcd"
>>> init [0..5 :: Int]
...
... 'init' is working with 'NonEmpty', not ordinary lists.
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
... 'init' is working with 'NonEmpty Char' lists
...
-}
init :: IsNonEmpty f a [a] "init" => f a -> [a]
init = NE.init

{- | @O(n)@. Extracts the last element of a 'NonEmpty' list.

>>> last ('a' :| "bcde")
'e'
>>> last [0..5 :: Int]
...
... 'last' is working with 'NonEmpty', not ordinary lists.
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
... 'last' is working with 'NonEmpty Char' lists
...
-}
last :: IsNonEmpty f a a "last" => f a -> a
last = NE.last

{- | @O(1)@. Return all the elements of a 'NonEmpty' list after the head
element.

>>> tail ('a' :| "bcde")
"bcde"
>>> tail [0..5 :: Int]
...
... 'tail' is working with 'NonEmpty', not ordinary lists.
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
... 'tail' is working with 'NonEmpty Char' lists
...
-}
tail :: IsNonEmpty f a [a] "tail" => f a -> [a]
tail = NE.tail
