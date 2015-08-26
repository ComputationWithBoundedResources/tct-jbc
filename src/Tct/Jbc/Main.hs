-- | This module provides the default executable tct-jbc.
module Main (main) where

import Tct.Core (setMode)
import Tct.Jbc  (jbcMode)

main :: IO ()
main = setMode jbcMode

