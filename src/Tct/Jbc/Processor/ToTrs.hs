module Tct.Jbc.Processor.ToTrs (toTrs, toTrsNarrowed) where


import qualified Data.IntSet                        as IS
import qualified Data.Set                           as S

import qualified Data.Rewriting.Problem             as RT
import qualified Data.Rewriting.Rule                as RT hiding (vars)
import qualified Data.Rewriting.Term                as RT hiding (varsDL)

import qualified Jat.Constraints                    as J (PAFun (..), PAFun (..), PAVar (..))

import           Tct.Core
import           Tct.Core.Processor.Transform       (transform)

import qualified Tct.Trs                            as R
import qualified Tct.Trs.Data.Problem               as R
import qualified Tct.Trs.Encoding.ArgumentFiltering as AF

import           Tct.Jbc.Data.Problem
import qualified Jat.Utils.TRS                      as J (simplifyTRS)
import           Tct.Jbc.Encoding.ArgumentFiltering (mkFilter)

-- import qualified Tct.Core.Common.Pretty             as PP
-- import Debug.Trace

--- * TRS ------------------------------------------------------------------------------------------------------------
-- transformation from CTRSs to TRSs
toTrs :: Strategy CTrs R.Trs
toTrs = transform "We extract a TRS fragment from the current cTRS problem" (toTrs' NoNarrow)

toTrsNarrowed :: Strategy CTrs R.Trs
toTrsNarrowed = transform "We extract a TRS fragment from the current cTRS problem" (toTrs' Narrow)

data Narrow = Narrow | NoNarrow deriving Eq

toTrs' :: Narrow -> CTrs -> Either String R.Trs
-- toTrs' = R.fromRewriting .  (\p -> trace (PP.display $ PP.pretty p) p ) . toRewriting where
toTrs' n = R.fromRewriting . toRewriting where
  toRewriting (CTrs gr rs) = RT.Problem
    { RT.startTerms = RT.BasicTerms
    , RT.strategy   = RT.Innermost
    , RT.theory     = Nothing
    , RT.rules      = RT.RulesPair
      { RT.strictRules = rules
      , RT.weakRules   = [] }
    , RT.variables  = []
    , RT.symbols    = []
    , RT.comment    = Just "greetings from jat" }
    where 
      rules = projectUniv . withNarrowing . elimFreshVars . elimTheory . fst $ unzip rs
      withNarrowing = if n == Narrow then narrowing else id
      narrowing rs' = fst . unzip . J.simplifyTRS gr $ map (\r -> (r,[])) rs'

projectUniv :: [RT.Rule J.PAFun J.PAVar] -> [RT.Rule String String]
projectUniv =  map (\(RT.Rule l r) -> RT.Rule (filterUniv1 l) (filterUniv1 r))
  where
    filterUniv1 (RT.Fun (J.UFun s) fs) = RT.Fun s (concatMap filterUniv2 fs)
    filterUniv1 _                      = error "not a fun"
    filterUniv2 (RT.Fun (J.UFun s) fs) = [RT.Fun s (concatMap filterUniv2 fs)]
    filterUniv2 (RT.Var (J.UVar s i))  = [RT.Var (s ++ show i)]
    filterUniv2 _                      = []

-- argument filter on argument positions that are not PA terms
elimTheory :: [RT.Rule J.PAFun J.PAVar] -> [RT.Rule J.PAFun J.PAVar]
elimTheory rs = AF.filterRule (univFilter rs) `fmap` rs

univFilter :: [RT.Rule J.PAFun J.PAVar] -> AF.ArgumentFiltering J.PAFun
univFilter = mkFilter isUTerm where
  isUTerm (RT.Fun (J.UFun _) _) = True
  isUTerm (RT.Var (J.UVar _ _)) = True
  isUTerm _                     = False

-- argument filter on argument positions that contain fresh variables on rhs
-- > elimFreshVars f(List(x),y) -> g(List(x'),y') == f() -> g()
elimFreshVars :: (Ord f, Ord v, Show v, Show f) => [RT.Rule f v] -> [RT.Rule f v]
elimFreshVars rs = let rs' = AF.filterRule (freshVarFilter rs) `fmap` rs in if rs == rs' then rs else elimFreshVars rs'

freshVarFilter :: (Ord f, Ord v) => [RT.Rule f v]  -> AF.ArgumentFiltering f
freshVarFilter = foldr k AF.empty where
  k (RT.Rule l (RT.Fun r rs)) af = AF.alter (add l rs) r af
  k _ af                         = af
  add l ts (Just (AF.Filtering is)) = Just . AF.Filtering $ is `IS.intersection` freshs (varsS l) ts
  add l ts _                        = Just . AF.Filtering $ freshs (varsS l) ts
  freshs vs = IS.fromAscList . fst . unzip . filter (not . isFresh vs . snd) . zip [(1::Int)..]
  isFresh vs = not . (`S.isSubsetOf` vs) . varsS
  varsS =  S.fromList . RT.vars

-- withTrivialTrs :: T.Strategy R.TrsProblem R.TrsProblem -> T.Strategy R.TrsProblem R.TrsProblem
-- withTrivialTrs st = st -- MS: whats an easy non-termination argument for our problems

