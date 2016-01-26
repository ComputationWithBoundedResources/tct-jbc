{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- | This module provides the default executable tct-jbc.
module Main (main) where

import Tct.Core
import Tct.Its
import Tct.Trs
import Tct.Jbc

import Tct.Core.Interactive

instance Declared Its Its where decls = itsDeclarations
instance Declared Trs Trs where decls = dpint:trsDeclarations
instance Declared Jbc Jbc where decls = jbcDeclarations


dpint :: StrategyDeclaration Trs Trs
dpint = SD $ strategy "dpint" () (dependencyPairs .>>> usableRules .>>> dpsimps .>>> ints 0 1)

main :: IO ()
main = runJbc jbcConfig

