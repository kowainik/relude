{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright:  (c) 2018-2019 Kowainik
License:    MIT
Maintainer: Kowainik <xrom.xkov@gmail.com>

Contains useful utilities to work with Types.
-}

module Relude.Extra.Type
       ( typeName
       , type (++)
       , AllHave
       , Elem
       , Fst
       , Snd
       ) where

import GHC.TypeLits (ErrorMessage (..), TypeError)
import Relude

#if ( __GLASGOW_HASKELL__ >= 802 )
import Type.Reflection (typeRep)
#else
import Data.Typeable (typeRep)
#endif


-- $setup
-- >>> :set -XDataKinds -XTypeOperators

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
-}
typeName :: forall a. Typeable a => Text
#if ( __GLASGOW_HASKELL__ >= 802 )
typeName = show (typeRep @a)
#else
typeName = show (typeRep (Proxy @a))
#endif
{-# INLINE typeName #-}

{- | Concatenates type-level lists.

>>> :kind! '[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[ 'Just 5, 'Nothing] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe
                                                              Nat]
= '[ 'Just 5, 'Nothing, 'Just 3, 'Nothing, 'Just 1]
>>> :kind! '[] ++ '[ 'Just 3, 'Nothing, 'Just 1]
'[] ++ '[ 'Just 3, 'Nothing, 'Just 1] :: [Maybe Nat]
= '[ 'Just 3, 'Nothing, 'Just 1]
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
    Snd '(_, y) = y
    Snd  (_, y) = y


{- | Check that a value is an element of a list:

>>> ok (Proxy :: Proxy (Elem Bool '[Int, Bool]))
OK

>>> ok (Proxy :: Proxy (Elem String '[Int, Bool]))
...
... [Char]...'[Int, Bool...
...
-}
type Elem e es = ElemGo e es es

-- 'orig' is used to store original list for better error messages
type family ElemGo e es orig :: Constraint where
  ElemGo x (x ': xs) orig = ()
  ElemGo y (x ': xs) orig = ElemGo y xs orig
  -- Note [Custom Errors]
  ElemGo x '[] orig       = TypeError ('ShowType x
                                 ':<>: 'Text " expected in list "
                                 ':<>: 'ShowType orig)
