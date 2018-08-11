{- | Contains useful functions to work with GHC callstack.
-}

module Relude.Extra.CallStack
       ( ownName
       ) where

import Relude

import GHC.Stack (HasCallStack, callStack, getCallStack)

-- TODO: better name?
{- | This function returns name of the caller for this function. But it requires
for caller to have 'HasCallStack' constraint. Otherwise it returns @"ownName".

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
