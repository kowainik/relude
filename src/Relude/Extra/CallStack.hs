{-# LANGUAGE Safe #-}

{- |
Copyright:  (c) 2018-2020 Kowainik
SPDX-License-Identifier: MIT
Maintainer:  Kowainik <xrom.xkov@gmail.com>
Stability:   Experimental
Portability: Portable

Contains useful functions to work with GHC callstack.

@since 0.2.0
-}

module Relude.Extra.CallStack
    ( ownName
    , callerName
    ) where

import Relude

{- | This function returns the name of its caller function, but it requires
that the caller function has 'HasCallStack' constraint. Otherwise, it returns
@"<unknown>"@.

>>> foo :: HasCallStack => String; foo = ownName
>>> foo
"foo"
>>> bar :: HasCallStack => String; bar = foo
>>> bar
"foo"

@since 0.2.0
-}
ownName :: HasCallStack => String
ownName = case getCallStack callStack of
    _:caller:_ -> fst caller
    _          -> "<unknown>"

{- | This function returns the name of its caller of the caller function, but it
requires that the caller function and caller of the caller function have
'HasCallStack' constraint. Otherwise, it returns @"<unknown>"@. It's useful for
logging:

>>> log :: HasCallStack => String -> IO (); log s = putStrLn $ callerName ++ ":" ++ s
>>> greeting :: HasCallStack => IO (); greeting = log "Starting..." >> putStrLn "Hello!" >> log "Ending..."
>>> greeting
greeting:Starting...
Hello!
greeting:Ending...

@since 0.2.0
-}
callerName :: HasCallStack => String
callerName = case getCallStack callStack of
    _:_:caller:_ -> fst caller
    _            -> "<unknown>"
