{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE DataKinds     #-}

{- |
Copyright:  (c) 2019 TheMatten
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains useful utilities for type-level programming.
-}

module Relude.Extra.Types
       ( type (++)
       , AllHave
       , Fst
       , Snd
       ) where

import Relude

-- $setup
-- >>> :set -XDataKinds -XTypeOperators

infixr 5 ++
{- | Concatenates type-level lists.

>>> :kind! '[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe
                                                              Nat]
= '[ 'Just 5, 'Nothing, 'Just 3, 'Nothing, 'Just 1]
>>> :kind! '[] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe Nat]
= '[ 'Just 3, 'Nothing, 'Just 1]
-}
type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    '[]       ++ ys = ys
    (x ': xs) ++ ys = x ': xs ++ ys

{- | Builds combined 'Constraint' by applying Constraint constructor to all
elements of type-level list.

>>> :kind! AllHave Show '[Int, Text, Double]
AllHave Show '[Int, Text, Double] :: Constraint
= (Show Int, (Show Text, (Show Double, () :: Constraint)))

which is equivalent to:

@
(Show Int, Show Text, Show Double) :: Constraint
@
-}
type family AllHave (f :: k -> Constraint) (xs :: [k]) :: Constraint where
    AllHave _ '[]       = ()
    AllHave f (x ': xs) = (f x, AllHave f xs)

{- | Returns first element of tuple type (with kind @*@) or type-level tuple
(with kind @(k1, k2)@, marked by prefix quote).

>>> :kind! Maybe (Fst '(Int, Text))
Maybe (Fst '(Int, Text)) :: *
= Maybe Int
>>> :kind! Maybe (Fst (Int, Text))
Maybe (Fst (Int, Text)) :: *
= Maybe Int
-}
type family Fst (t :: k) :: k' where
    Fst '(x, _) = x
    Fst  (x, _) = x

{- | Returns second element of tuple type (with kind @*@) or type-level tuple
(with kind @(k1, k2)@, marked by prefix quote).

>>> :kind! Maybe (Snd '(Int, Text))
Maybe (Snd '(Int, Text)) :: *
= Maybe Text
>>> :kind! Maybe (Snd (Int, Text))
Maybe (Snd (Int, Text)) :: *
= Maybe Text
-}
type family Snd (t :: k) :: k' where
    Fst '(_, y) = y
    Fst  (_, y) = y
