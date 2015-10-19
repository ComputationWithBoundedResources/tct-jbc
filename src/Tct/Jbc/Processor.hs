{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tct.Jbc.Processor
  ( module M

  , jbcDeclarations
  , itsArg
  , trsArg
  , Narrow (..)
  , narrowArg

  , jbcStrategy
  , jbcDeclaration
  , trsStrategy
  , trsDeclaration
  , itsStrategy
  , itsDeclaration
  ) where


import qualified Data.Foldable           as F
import           Data.Typeable

import           Tct.Core
import qualified Tct.Core.Common.Parser  as TP
import qualified Tct.Core.Data           as T
import qualified Tct.Core.Parse          as TP

import qualified Tct.Trs                 as R
import qualified Tct.Trs.Data            as R
import qualified Tct.Trs.Declarations    as R

import qualified Tct.Its                 as I
import qualified Tct.Its.Processor       as I
import qualified Tct.Its.Strategy        as I


import           Tct.Jbc.Data
import           Tct.Jbc.Method.ToCTrs   as M
import           Tct.Jbc.Method.ToIts    as M
import           Tct.Jbc.Method.ToTrs    as M


jbcDeclarations :: [StrategyDeclaration Jbc Jbc]
jbcDeclarations =
  [ T.SD jbcDeclaration
  , T.SD itsDeclaration
  , T.SD trsDeclaration ]

jbcDeclaration = T.strategy "jbc" (itsArg `optional` I.runtime, trsArg `optional` R.competition) jbcStrategy
itsDeclaration = T.strategy "its" (OneTuple $ itsArg `optional` I.runtime)                       itsStrategy
trsDeclaration = T.strategy "trs" (narrowArg `optional` Narrow, trsArg `optional` R.competition) trsStrategy

instance TP.SParsable Jbc o R.TrsStrategy where
  parseS = TP.withState R.trsDeclarations TP.strategy

instance TP.SParsable Jbc o (Strategy I.Its I.Its) where
  parseS = TP.withState [T.SD I.runtimeDeclaration] TP.strategy

jbcStrategy ::
  Strategy I.Its I.Its
  -> Strategy R.TrsProblem R.TrsProblem
  -> Strategy Jbc Jbc
jbcStrategy stits sttrs =
  toCTrs .>>> fastest
    [ toTrsNarrowed .>>> withTrivialTrs sttrs .>>> abort
    , toIts         .>>> withTrivialIts stits .>>> abort ]


--- * its ------------------------------------------------------------------------------------------------------------

withTrivialIts :: T.Strategy I.Its I.Its -> T.Strategy I.Its I.Its
withTrivialIts st = withProblem $ \prob ->
  if noConstraints prob then I.boundTrivialSCCs .>>> I.empty else st
  where noConstraints prob = all (null . I.con) (F.toList $ I._irules prob)

itsStrategy :: Strategy I.Its I.Its -> Strategy Jbc Jbc
itsStrategy stits = toCTrs .>>> toIts .>>> withTrivialIts stits .>>> abort


--- * trs ------------------------------------------------------------------------------------------------------------

data Narrow = Narrow | NoNarrow
  deriving (Bounded, Enum, Eq, Typeable, Show)

instance TP.SParsable i o Narrow where
  parseS = TP.enum

withTrivialTrs :: Strategy R.TrsProblem R.TrsProblem -> Strategy R.TrsProblem R.TrsProblem
withTrivialTrs = id

trsStrategy :: Narrow -> Strategy R.TrsProblem R.TrsProblem -> JbcStrategy
trsStrategy Narrow sttrs   = toCTrs .>>> toTrsNarrowed .>>> withTrivialTrs sttrs .>>> abort
trsStrategy NoNarrow sttrs = toCTrs .>>> toTrs         .>>> withTrivialTrs sttrs .>>> abort


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

