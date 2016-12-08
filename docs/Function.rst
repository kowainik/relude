Functions
=========

.. highlight:: haskell

$
***

::

  ($) :: (a -> b) -> a -> b

&
***

::

  (&) :: a -> (a -> b) -> b

.
***

::

  (.) :: (b -> c) -> (a -> b) -> a -> c


flip
****

::

  flip :: (a -> b -> c) -> b -> a -> c


on
***

::

  on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

const
*****

::

  const :: a -> b -> a

fix
***

::

  fix :: (a -> a) -> a

identity
********

::

  identity :: a -> a

$!
***

::

  ($!) :: NFData a => (a -> b) -> a -> b

$!!
***

::

  ($!!) :: NFData a => (a -> b) -> a -> b

force
*****

::

  force :: NFData a => a -> a
