{-|
Module      : System.Console.Terminal
Copyright   : (c) Craig Roche 2013
License     : BSD-style

Maintainer  : cdxr01@gmail.com
Stability   : experimental
Portability : non-portable


This module re-exports the 'MonadTerm' typeclass and the 'TermT'
implementation. Most users will only need to import this module.
-}

module System.Console.Terminal
( module System.Console.Terminal.Class
, module System.Console.Terminal.Term
) where


import System.Console.Terminal.Class
import System.Console.Terminal.Term
