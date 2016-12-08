Tuples
======

.. highlight:: haskell

fst
***

::

  fst :: (a, b) -> a

Extract the first component of a pair.

*Example*:

::
  
  > fst (1,2)
  1

snd
***

::

  snd :: (a, b) -> b

Extract the second component of a pair.

*Example*:

::
  
  > snd (1,2)
  2

swap
****

::

  swap :: (a, b) -> (b, a)

Swap the components of a pair.

*Example*:

::
  
  > swap (1,2)
  (2,1)

curry
*****

::

  curry :: ((a, b) -> c) -> a -> b -> c

curry converts an uncurried function to a curried function.

*Example*:

::

  > curry fst 1 2
  1

uncurry
*******

::

  uncurry :: (a -> b -> c) -> (a, b) -> c

uncurry converts a curried function to a function on pairs.

*Example*:

::

  > uncurry (+) (1,2)
  3
