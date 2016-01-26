{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Tct.Jbc.Strategies
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
  , ctrsStrategy
  , ctrsDeclaration
  ) where


import qualified Data.Foldable           as F
import           Data.Typeable

import           Tct.Core
import qualified Tct.Core.Data           as T

import qualified Tct.Trs                 as R

import qualified Tct.Its                 as I
import qualified Tct.Its.Data.Rule       as I

import           Tct.Jbc.Data.Problem
import           Tct.Jbc.Processor.ToCTrs   as M
import           Tct.Jbc.Processor.ToIts    as M
import           Tct.Jbc.Processor.ToTrs    as M
import           Tct.Jbc.Processor.PolynomialInterpretation    as M


jbcDeclarations :: (Declared I.Its I.Its, Declared R.Trs R.Trs) => [StrategyDeclaration Jbc Jbc]
jbcDeclarations =
  [ T.SD jbcDeclaration
  , T.SD itsDeclaration
  , T.SD trsDeclaration
  , T.SD ctrsDeclaration ]

jbcDeclaration :: (Declared I.Its I.Its, Declared R.Trs R.Trs) => 
  T.Declaration ('[T.Argument 'T.Optional I.ItsStrategy, T.Argument 'T.Optional R.TrsStrategy] T.:-> Strategy Jbc Jbc)
jbcDeclaration = T.strategy "jbc" (itsArg `optional` I.runtime, trsArg `optional` R.competition) jbcStrategy

itsDeclaration :: Declared I.Its I.Its => 
  T.Declaration ('[T.Argument 'T.Optional I.ItsStrategy] T.:-> Strategy Jbc Jbc)
itsDeclaration = T.strategy "its" (OneTuple $ itsArg `optional` I.runtime)                       itsStrategy

trsDeclaration :: Declared R.Trs R.Trs => 
  T.Declaration ('[T.Argument 'T.Optional Narrow, T.Argument 'T.Optional R.TrsStrategy] T.:-> Strategy Jbc Jbc)
trsDeclaration = T.strategy "trs" (narrowArg `optional` Narrow, trsArg `optional` R.competition) trsStrategy

ctrsDeclaration :: T.Declaration ('[T.Argument 'T.Optional T.Nat, T.Argument 'T.Optional T.Nat] T.:-> Strategy Jbc Jbc)
ctrsDeclaration = T.strategy "ctrs" R.boundedArgs ctrsStrategy

jbcStrategy ::
  Strategy I.Its I.Its
  -> Strategy R.Trs R.Trs
  -> Strategy Jbc Jbc
jbcStrategy stits sttrs =
  toCTrs .>>> fastest
    [ toTrsNarrowed .>>> withTrivialTrs sttrs .>>> abort
    , toIts         .>>> withTrivialIts stits .>>> abort
    , toCTrs'       .>>> polys 0 2 .>>> abort ]


--- * ctrs -----------------------------------------------------------------------------------------------------------

ctrsStrategy :: Int -> Int -> JbcStrategy
ctrsStrategy l u = toCTrs .>>> toCTrs' .>>> polys l u .>>> close


--- * its ------------------------------------------------------------------------------------------------------------

withTrivialIts :: T.Strategy I.Its I.Its -> T.Strategy I.Its I.Its
withTrivialIts st = withProblem $ \prob ->
  if noConstraints prob then I.boundTrivialSCCs .>>> I.empty else st
  where noConstraints prob = all (null . I.con) (F.toList $ I.irules_ prob)

itsStrategy :: Strategy I.Its I.Its -> Strategy Jbc Jbc
itsStrategy stits = toCTrs .>>> toIts .>>> withTrivialIts stits .>>> abort


--- * trs ------------------------------------------------------------------------------------------------------------

data Narrow = Narrow | NoNarrow
  deriving (Bounded, Enum, Eq, Typeable, Show)

withTrivialTrs :: Strategy R.Trs R.Trs -> Strategy R.Trs R.Trs
withTrivialTrs = id

trsStrategy :: Narrow -> Strategy R.Trs R.Trs -> JbcStrategy
trsStrategy Narrow sttrs   = toCTrs .>>> toTrsNarrowed .>>> withTrivialTrs sttrs .>>> abort
trsStrategy NoNarrow sttrs = toCTrs .>>> toTrs         .>>> withTrivialTrs sttrs .>>> abort


--- * declaration ----------------------------------------------------------------------------------------------------

trsArg :: Declared R.Trs R.Trs => T.Argument 'T.Required (Strategy R.Trs R.Trs)
trsArg = T.strat "trs" [ "This argument specifies the trs strategy to apply." ]

itsArg :: Declared I.Its I.Its => T.Argument 'T.Required (Strategy I.Its I.Its)
itsArg = T.strat "its" [ "This argument specifies the its strategy." ]

narrowArg :: T.Argument 'T.Required Narrow
narrowArg = T.flag "narrow" ["Wether narrowing/forward chaining/composition should be applied."]

