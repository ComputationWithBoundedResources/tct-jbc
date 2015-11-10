-- | This module provides the default executable tct-jbc.
module Main (main) where

import Tct.Core
import Tct.Its
import Tct.Trs
import Tct.Jbc

instance Declared Its Its where decls = itsDeclarations
instance Declared Trs Trs where decls = trsDeclarations
instance Declared Jbc Jbc where decls = jbcDeclarations

main :: IO ()
main = runJbc jbcConfig

