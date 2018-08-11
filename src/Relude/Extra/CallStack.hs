{- | Contains useful functions to work with GHC callstack.
-}

module Relude.Extra.CallStack
       ( ownName
       ) where

import Relude

import GHC.Stack (HasCallStack, callStack, getCallStack)

-- TODO: better name?
{- | This function returns the name of its caller function, but it requires
that the caller function has 'HasCallStack' constraint. Otherwise, it returns
@"ownName"@.

>>> foo :: HasCallStack => String; foo = ownName
>>> foo
"foo"
>>> bar :: HasCallStack => String; bar = foo
>>> bar
"foo"
-}
ownName :: HasCallStack => String
ownName = case getCallStack callStack of
    _:caller:_ -> fst caller
    _          -> "ownName"
