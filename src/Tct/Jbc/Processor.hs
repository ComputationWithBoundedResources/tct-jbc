module Tct.Jbc.Processor
  ( module M

  , defaultDeclarations
  , itsArg
  , trsArg
  , Narrow (..)
  , narrowArg

  , jatStrategy
  , trsStrategy
  , itsStrategy
  ) where


import qualified Data.Foldable           as F
import           Data.Typeable

import           Tct.Core
import qualified Tct.Core.Common.Parser  as P
import qualified Tct.Core.Data           as T
import qualified Tct.Core.Parse          as P
import qualified Tct.Core.Processor.Cast as T

import qualified Tct.Trs                 as R

import qualified Tct.Its                 as I
import qualified Tct.Its.Processor       as I


import           Tct.Jbc.Data
import           Tct.Jbc.Method.ToCTrs   as M
import           Tct.Jbc.Method.ToIts    as M
import           Tct.Jbc.Method.ToTrs    as M


defaultDeclarations :: [StrategyDeclaration Jbc Jbc]
defaultDeclarations = []

jatStrategy ::
  Strategy R.TrsProblem R.TrsProblem
  -> Strategy I.Its I.Its
  -> Strategy Jbc Jbc
jatStrategy sttrs stits =
  toCTrs >=> fastest
    [ toTrsNarrowed >=> withTrivialTrs sttrs >=> T.close
    , toIts         >=> withTrivialIts stits >=> T.close ]


--- * its ------------------------------------------------------------------------------------------------------------

withTrivialIts :: T.Strategy I.Its I.Its -> T.Strategy I.Its I.Its
withTrivialIts st = withProblem $ \prob ->
  if noConstraints prob then I.boundTrivialSCCs >>> I.empty else st
  where noConstraints prob = all (null . I.con) (F.toList $ I._irules prob)

itsStrategy :: Strategy I.Its I.Its -> Strategy Jbc Jbc
itsStrategy stits = toCTrs >=> toIts >=> withTrivialIts stits >=> T.close


--- * trs ------------------------------------------------------------------------------------------------------------

data Narrow = Narrow | NoNarrow
  deriving (Bounded, Enum, Eq, Typeable, Show)

instance P.SParsable i i Narrow where
  parseS = P.enum

withTrivialTrs :: Strategy R.TrsProblem R.TrsProblem -> Strategy R.TrsProblem R.TrsProblem
withTrivialTrs = id

trsStrategy :: Narrow -> Strategy R.TrsProblem R.TrsProblem -> JbcStrategy
trsStrategy Narrow sttrs   = toCTrs >=> toTrsNarrowed >=> withTrivialTrs sttrs >=> T.close
trsStrategy NoNarrow sttrs = toCTrs >=> toTrs         >=> withTrivialTrs sttrs >=> T.close


--- * declaration ----------------------------------------------------------------------------------------------------

trsArg :: T.Argument 'T.Required (Strategy R.TrsProblem R.TrsProblem)
trsArg = T.arg
  `T.withName` "trs"
  `T.withHelp` [ "This argument specifies the trs strategy to apply." ]

itsArg :: T.Argument 'T.Required (Strategy I.Its I.Its)
itsArg = T.arg
  `T.withName` "its"
  `T.withHelp` [ "This argument specifies the its strategy." ]

narrowArg :: T.Argument 'T.Required Narrow
narrowArg = T.arg
  `T.withHelp` []
  `T.withDomain` fmap show [(minBound :: Narrow)..]

