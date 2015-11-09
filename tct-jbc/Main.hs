-- | This module provides the default executable tct-jbc.
module Main (main) where

import Tct.Jbc.Config
import Tct.Jbc.Data.Problem

import Tct.Core
import Tct.Its
import Tct.Trs.Declarations
import Tct.Trs

instance Declared Its Its               where decls = itsDeclarations
instance Declared TrsProblem TrsProblem where decls = trsDeclarations
instance Declared Jbc Jbc               where decls = []

main :: IO ()
main = runJbc jbcConfig

