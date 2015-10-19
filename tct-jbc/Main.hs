-- | This module provides the default executable tct-jbc.
module Main (main) where

import Tct.Jbc.Config

main :: IO ()
main = runJbc jbcConfig

