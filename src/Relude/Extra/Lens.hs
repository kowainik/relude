{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE Safe       #-}

{- |
Module                  : Relude.Extra.Lens
Copyright               : (c) 2012-2016 Edward Kmett
                          (c) 2019-2023 Kowainik
SPDX-License-Identifier : MIT
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@since 0.5.0

This module aims to provide a minimal implementation of @lens@ package required
for basic usage. All functions are compatible with the real @lens@ package
therefore if you need to expand to the full version the process should be
straightforward.

Main ideas implemented in this module are described in the following blog post:

* [Write yourself a lens (by Veronika Romashkina)](https://vrom911.github.io/blog/write-yourself-a-lens)

== Usage

To use lenses in your project, you don't need to add any other dependency rather
than @relude@. You should add the import of this module in the place of lenses
usage:

@
__import__ Relude.Extra.Lens
@

== Example

To understand better how to use this module lets look at some simple example.
Let's say we have the user data type in our system:

@
__data__ User = User
    { userName    :: 'Text'
    , userAge     :: 'Int'
    , userAddress :: Address
    } __deriving__ ('Show')

__data__ Address = Address
    { addressCountry :: 'Text'
    , addressCity    :: 'Text'
    , addressIndex   :: 'Text'
    } __deriving__ ('Show')
@

To create the lens for the @userName@ field we can use 'lens' function and manually writing getter and setter function:

@
nameL :: 'Lens'' User 'Text'
nameL = 'lens' getter setter
  __where__
    getter :: User -> 'Text'
    getter = userName

    setter :: User -> 'Text' -> User
    setter user newName = user {userName = newName}
@

In this manner, we can create other lenses for our User data type.

@
ageL     :: 'Lens'' User 'Int'
addressL :: 'Lens'' User Address
countryL :: 'Lens'' User 'Text'
cityL    :: 'Lens'' User 'Text'
indexL   :: 'Lens'' User 'Text'
@

/Note:/ here we are using composition of the lenses for @userAddress@ field. If we have

@
addressCityL :: 'Lens'' Address 'Text'
@

then

@
cityL = addressL . addressCityL
@

Let's say we have some sample user

@
user :: User
user = User
    { userName = \"John\"
    , userAge  = 42
    , userAddress = Address
        { addressCountry = \"UK\"
        , addressCity    = \"London\"
        , addressIndex   = \"XXX\"
        }
    }
@

To view the fields of the User data type we can use 'view' or '^.'

@
__>>>__ 'view' ageL user
42
__>>>__ user '^.' cityL
\"London\"
@

If we want to change any of the user's data, we should use 'set' or '.~'

@
__>>>__ 'set' nameL \"Johnny\" user
__>>>__ user '&' indexL '.~' \"YYY\"
@

'over' or '%~' operator could be useful when, for example, you want to increase the age by one on the user's birthday:

@
__>>>__ 'over' ageL 'succ' user
__>>>__ user '&' ageL '%~' 'succ'
@

== Migration

This module is not supposed to be the replacement for the @lens@ package. One of
the reasons why one would want to migrate to @lens@ or @microlens@ is that the
functional in @relude@ is limited to just vital lens functions.

To migrate to @lens@ or @microlens@ package add the required library to the
dependencies list in the @.cabal@ file and replace the import from @relude@
library

@
__import__ Relude.Extra.Lens
@

to the one of this correspondingly:

- @lens@:

    @
    __import__ Control.Lens
    @

- @microlens@:

    @
    __import__ Lens.Micro
    @

And that's all! No need to change the types or implementation of the functions
you used @Relude.Extra.Lens@ in.

== Links

- [lens](https://hackage.haskell.org/package/lens)
- [microlens](https://hackage.haskell.org/package/microlens)
- [lens tutorial](http://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html)
-}

module Relude.Extra.Lens
    ( Lens'
    , lens
    , view
    , set
    , over
    , (^.)
    , (.~)
    , (%~)
    ) where

import Relude


{- | The monomorphic lenses which don't change the type of the container (or of
the value inside). It has a 'Functor' constraint, and since both 'Const' and
'Identity' are functors, it can be used whenever a getter or a setter is needed.

  * @a@ is the type of the value inside of structure
  * @s@ is the type of the whole structure

@since 0.5.0
-}
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

{- | Creates 'Lens'' from the getter and setter.

@since 0.5.0
-}
lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter = \f s -> setter s <$> f (getter s)
{-# INLINE lens #-}

{- | Gets a value out of a structure using a getter.

@since 0.5.0
-}
view :: Lens' s a -> s -> a
view l = getConst . l Const
{-# INLINE view #-}

{- | Sets the given value to the structure using a setter.

@since 0.5.0
-}
set :: Lens' s a -> a -> s -> s
set l a = runIdentity . l (const (Identity a))
{-# INLINE set #-}

{- | Applies the given function to the target.

@since 0.5.0
-}
over :: Lens' s a -> (a -> a) -> s -> s
over l fa = runIdentity . l (Identity . fa)
{-# INLINE over #-}

{- | The operator form of 'view' with the arguments flipped.

@since 0.5.0
-}
infixl 8 ^.
(^.) :: s -> Lens' s a -> a
s ^. l = view l s
{-# INLINE (^.) #-}

{- | The operator form of 'set'.

@since 0.5.0
-}
infixr 4 .~
(.~) :: Lens' s a -> a -> s -> s
(.~) = set
{-# INLINE (.~) #-}

{- | The operator form of 'over'.

@since 0.5.0
-}
infixr 4 %~
(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over
{-# INLINE (%~) #-}


{-
Copyright 2012-2016 Edward Kmett

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}
