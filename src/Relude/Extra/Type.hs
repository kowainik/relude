{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright:  (c) 2018-2021 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

Contains useful utilities to work with Types.

@since 0.4.0
-}

module Relude.Extra.Type
    ( typeName
    , type (++)
    , AllHave
    , Elem
    , Fst
    , Snd
    ) where

import Relude

import Type.Reflection (typeRep)


-- $setup
-- >>> :set -XAllowAmbiguousTypes -XDataKinds -XExplicitNamespaces -XPolyKinds -XTypeFamilies -XTypeOperators -XUndecidableInstances 

{- | Gets a string representation of a type.

__NOTE:__ This must be used with __TypeApplications__ language extension.

>>> typeName @()
"()"
>>> typeName @Int
"Int"
>>> typeName @String
"[Char]"
>>> typeName @(Maybe Int)
"Maybe Int"

@since 0.4.0
-}
typeName :: forall a. Typeable a => Text
typeName = show (typeRep @a)
{-# INLINE typeName #-}

{- | Concatenates type-level lists.

#if ( __GLASGOW_HASKELL__ >= 902 )

>>> :kind! '[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe
                                                              Natural]
= '[ 'Just 5, 'Nothing, 'Just 3, 'Nothing, 'Just 1]

>>> :kind! '[] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe Natural]
= '[ 'Just 3, 'Nothing, 'Just 1]

#elif ( __GLASGOW_HASKELL__ >= 806 )

>>> :kind! '[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe
                                                              Nat]
= '[ 'Just 5, 'Nothing, 'Just 3, 'Nothing, 'Just 1]

>>> :kind! '[] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe Nat]
= '[ 'Just 3, 'Nothing, 'Just 1]

#endif

@since 0.6.0.0
-}
infixr 5 ++
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

@since 0.6.0.0
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

@since 0.6.0.0
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

@since 0.6.0.0
-}
type family Snd (t :: k) :: k' where
    Snd '(_, y) = y
    Snd  (_, y) = y

{- | Check that a type is an element of a list:

>>> :kind! Elem String '[]
Elem String '[] :: Bool
= 'False

>>> :kind! Elem String '[Int, String]
Elem String '[Int, String] :: Bool
= 'True

>>> :kind! Elem String '[Int, Bool]
Elem String '[Int, Bool] :: Bool
= 'False

@since 0.6.0.0
-}
type family Elem (e :: t) (es :: [t]) :: Bool where
    Elem _ '[]       = 'False
    Elem x (x ': xs) = 'True
    Elem x (_ ': xs) = Elem x xs
