module Tct.Jbc (jbcMode) where

import           Tct.Core              (withStrategies)
import           Tct.Core.Declarations (declarations)

import qualified Tct.Jbc.Data.Mode     as J
import           Tct.Jbc.Processor


-- | A default jbcMode
jbcMode :: J.JbcMode
jbcMode = J.jbcMode `withStrategies` (declarations ++ defaultDeclarations)

