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
import qualified Tct.Core.Data           as T

import qualified Tct.Trs                 as R
import qualified Tct.Trs.Data            as R
import qualified Tct.Trs.Declarations    as R

import qualified Tct.Its                 as I
import qualified Tct.Its.Data.Rule            as I
import qualified Tct.Its.Strategy        as I


import           Tct.Jbc.Data
import           Tct.Jbc.Method.ToCTrs   as M
import           Tct.Jbc.Method.ToIts    as M
import           Tct.Jbc.Method.ToTrs    as M


jbcDeclarations :: (Declared I.Its I.Its, Declared R.TrsProblem R.TrsProblem) => [StrategyDeclaration Jbc Jbc]
jbcDeclarations =
  [ T.SD jbcDeclaration
  , T.SD itsDeclaration
  , T.SD trsDeclaration ]

jbcDeclaration :: (Declared I.Its I.Its, Declared R.TrsProblem R.TrsProblem) => T.Declaration ('[T.Argument 'T.Optional I.ItsStrategy, T.Argument 'T.Optional R.TrsStrategy] T.:-> Strategy Jbc Jbc)
jbcDeclaration = T.strategy "jbc" (itsArg `optional` I.runtime, trsArg `optional` R.competition) jbcStrategy

itsDeclaration :: Declared I.Its I.Its => T.Declaration ('[T.Argument 'T.Optional I.ItsStrategy] T.:-> Strategy Jbc Jbc)
itsDeclaration = T.strategy "its" (OneTuple $ itsArg `optional` I.runtime)                       itsStrategy

trsDeclaration :: Declared R.TrsProblem R.TrsProblem => T.Declaration ('[T.Argument 'T.Optional Narrow, T.Argument 'T.Optional R.TrsStrategy] T.:-> Strategy Jbc Jbc)
trsDeclaration = T.strategy "trs" (narrowArg `optional` Narrow, trsArg `optional` R.competition) trsStrategy

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

withTrivialTrs :: Strategy R.TrsProblem R.TrsProblem -> Strategy R.TrsProblem R.TrsProblem
withTrivialTrs = id

trsStrategy :: Narrow -> Strategy R.TrsProblem R.TrsProblem -> JbcStrategy
trsStrategy Narrow sttrs   = toCTrs .>>> toTrsNarrowed .>>> withTrivialTrs sttrs .>>> abort
trsStrategy NoNarrow sttrs = toCTrs .>>> toTrs         .>>> withTrivialTrs sttrs .>>> abort


--- * declaration ----------------------------------------------------------------------------------------------------

trsArg :: Declared R.TrsProblem R.TrsProblem => T.Argument 'T.Required (Strategy R.TrsProblem R.TrsProblem)
trsArg = T.strat "trs" [ "This argument specifies the trs strategy to apply." ]

itsArg :: Declared I.Its I.Its => T.Argument 'T.Required (Strategy I.Its I.Its)
itsArg = T.strat "its" [ "This argument specifies the its strategy." ]

narrowArg :: T.Argument 'T.Required Narrow
narrowArg = T.flag "narrow" ["Wether narrowing/forward chaining/composition should be applied."]

