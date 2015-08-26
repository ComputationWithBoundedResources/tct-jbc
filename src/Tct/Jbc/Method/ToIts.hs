module Tct.Jbc.Method.ToIts (toIts) where


import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe)
import qualified Data.Set                           as S

-- import qualified J2J.Compiler                       as JJ (compileClasses, processClasses)
-- import qualified J2J.Datatypes.MetaTypes            as JJ (MapOfMapClasses)
-- import qualified J2J.Output.JinjaByteCode           as JJ (prettyJBC)
-- import qualified J2J.Output.JinjaSourceCode         as JJ (prettySourceCode)

import qualified Jat.CompGraph                      as J (JGraph)
import qualified Jat.Constraints                    as J (PAFun (..), PAFun (..), PAVar (..), bot, eq, isIFun, isIFun,
                                                          isRFun, not, pushNot, top)
import qualified Jat.Utils.TRS                      as J (PARule, prettyITS, simplifyTRS)

import           Tct.Core
import qualified Tct.Core.Combinators               as T
import qualified Tct.Core.Data                      as T

import qualified Data.Rewriting.Rule                as RT hiding (vars)

import qualified Tct.Trs.Encoding.ArgumentFiltering as AF

import qualified Tct.Its                            as I

import           Tct.Jbc.Data
import           Tct.Jbc.Encoding.ArgumentFiltering (mkFilter)

-- import Debug.Trace
-- import qualified Tct.Core.Common.Pretty as PP

--- * ITS ------------------------------------------------------------------------------------------------------------
-- transformation from CTRSs to the ITSs

toIts :: Strategy CTrs I.Its
toIts = T.transform (fromResult . toIts')
  where fromResult = either (error . show) T.Continue

toIts' :: CTrs -> Either String I.Its
toIts' (CTrs gr rs) =  I.fromString . show . J.prettyITS "a" $ toITS gr rs
-- toIts' (CTrs gr rs) =  I.fromString . traceId . PP.display . J.prettyITS "a" $ toITS gr rs

toITS :: J.JGraph i a -> [J.PARule] -> [J.PARule]
toITS gr =
  S.toList
  . S.fromList
  . addStartRule
  . map (normalizevars . linearise)
  -- . (\p -> trace (PP.display (J.prettyITS "f0" p)) p)
  . padding
  . elimUniv
  . map substituteBVal
  . simplify gr
  . concatMap (expandNeq . substituteIFun)
  . concatMap instantiateBVar
  where
    apply tau (RT.Rule (RT.Fun l ls) (RT.Fun r rs)) = let sigma = sig tau in RT.Rule (RT.Fun l $ sigma ls) (RT.Fun r $ sigma rs)
    apply _ _ = error "apply: not a rule"
    sig ls = map k  where
        k t@(RT.Fun _ _) = t
        k t = t `fromMaybe` lookup t ls
    top x       = sig [(x,J.top)]
    bot x       = sig [(x,J.bot)]


    -- we consider constraints: b := a1 * a2 where b is a boolean variable and * is either a relational operator between integer or a boolean operation
    -- (i) we instantiate the result with True/False;
    -- (iia) if the result is obtained from an Boolean operation; we suitably instantiate the arguments
    -- (iib) if the result is obtained from an relational operation; we consider the case and the negation of it
    -- f(X) -> f(Y) | b := x > 0    --> [f(X) -> f(Y) | x > 0 {b/True}, f(X) -> f(Y) | not(x > 0) {b /False}
    -- f(X) -> f(Y) | b := b1 && b2 --> [f(X) -> f(Y) {b/True, b1/True,/b2/True}, f(X) -> f(Y) {b/False,b1/False} ...
    instantiateBVar ( rule@(RT.Rule l (RT.Fun r rs)), [RT.Fun J.Ass [w@(RT.Var (J.BVar _ _)), f]] ) = case f of
      (RT.Fun J.And [b1,b2]) ->
        [ (apply [(b1,J.top),(b2,J.top),(w,J.top)] rule, [])
        , (apply [(b1,J.bot),            (w,J.bot)] rule, [])
        , (apply [            (b2,J.bot),(w,J.bot)] rule, []) ]
      (RT.Fun J.Or [b1,b2])  ->
        [ (apply [(b1,J.bot),(b2,J.bot),(w,J.bot)] rule, [])
        , (apply [(b1,J.top),            (w,J.top)] rule, [])
        , (apply [            (b2,J.top),(w,J.top)] rule, []) ]
      (RT.Fun J.Not [b1])                         ->
        [ (apply [(b1,J.bot),(w,J.top)] rule, [])
        , (apply [(b1,J.top),(w,J.bot)] rule, []) ]

      rel | J.isRFun rel -> [( RT.Rule l (RT.Fun r $ top w rs), [f]), ( RT.Rule l (RT.Fun r $ bot w rs), [J.pushNot $ J.not f])]
      _                   -> error "instantitateBVar: toITS mistyped?"
    instantiateBVar cr = [cr]

    -- we lift constraints that are operations over integers into the rhs
    -- f(X) -> f(Y) | x := x+1 --> f(X) -> f(Y {x/x+1})
    substituteIFun (RT.Rule l (RT.Fun r rs) ,[RT.Fun J.Ass [v@(RT.Var (J.IVar _ _)), t]])
      | J.isIFun t = (RT.Rule l (RT.Fun r $ sig [(v, t)] rs), [])
    substituteIFun cr = cr

    -- for some reason we do not accept the negation of equality; so we consider
    -- f(X) -> f(Y) |  x1 /= x2 --> f(X) -> f(Y) | x1 > x2, f(X) -> f(Y) | x1 < x2
    expandNeq (r,[RT.Fun J.Neq [t1,t2]]) =
      [ (r, [RT.Fun J.Gt [t1,t2]])
      , (r, [RT.Fun J.Gt [t2,t1]]) ]
    expandNeq r = [r]

    -- simplifying on rhs/inlining
    simplify = J.simplifyTRS

    -- we replace Boolean values with True False; this is sound value abstraction as all boolean operations are already handled by instantiateBVAr
    -- f(X) -> f(Y) C --> f(X) -> f(Y) C {True/1,False/0,null/0}
    substituteBVal (RT.Rule (RT.Fun l ls) (RT.Fun r rs), cr) = (RT.Rule (RT.Fun l (foldr k [] ls)) (RT.Fun r (foldr k [] rs)), cr)
      where
        k t
          | t == J.top = (RT.Fun (J.IConst 1) []:)
          | t == J.bot = (RT.Fun (J.IConst 0) []:)
          | otherwise   = (t:)
    substituteBVal _ = error "substituteBVal: not a rule"

    -- start rules do not have incoming edges; this could happend if the first satement is a loop
    addStartRule (r@(RT.Rule (RT.Fun _ as) _ ,_) :rs) = (RT.Rule (RT.Fun (J.UFun "a") as) (RT.Fun (J.UFun "f0") as),[]) :r:rs
    addStartRule _ = []

    -- environment variables are initialised with null, therefore we can have for example f1(x) -> f1(null); f1(x) -> f1(x+1)
    -- then af(f)={};  so we first ignore null; then replace it with 0;
    -- alternatively we could have replaced all non-theory terms with fresh variables; but then we have all thes fresh variables in the system
    elimUniv rs = [ (replaceNull $ af r,cs) | (r,cs) <- rs ]
      where af = AF.filterRule . theoryFilter . fst $ unzip rs

    theoryFilter = mkFilter (not . isUTerm)  where
      isUTerm (RT.Fun (J.UFun "null") []) = False
      isUTerm (RT.Fun (J.UFun _) _)       = True
      isUTerm (RT.Var (J.UVar _ _))       = True
      isUTerm _                           = False

    replaceNull (RT.Rule (RT.Fun l ls) (RT.Fun r rs)) = RT.Rule (RT.Fun l $ replaceNull' `fmap` ls) (RT.Fun r $ replaceNull' `fmap` rs) where
      replaceNull' (RT.Fun (J.UFun "null") []) = RT.Fun (J.IConst 0) []
      replaceNull' t                           = t
    replaceNull _ = error "replaceNull: not a rule"

    -- the ITS fromat requires that all function symbols have the same arity
    -- so we introduce fresh variables on lhs and 0 on rhs wrt to the max arity of the system
    padding rs = [ (c,cs) | (RT.Rule l r ,cs) <- rs, let c = RT.Rule (padLeft l) (padRight r)]
      where
        padLeft (RT.Fun s fs) = RT.Fun s (take m $ fs ++ varsl)
        padLeft _            = error "padding: oh no"
        padRight (RT.Fun s fs) = RT.Fun s (take m $ fs ++ varsr)
        padRight _            = error "padding: oh no"


        m = max 1 $ maximum [ n | (RT.Rule l r ,_) <- rs, let n = max (len l) (len r)]
        len (RT.Fun _ fs) = length fs
        len _            = 0
        varsl = map (RT.Var . J.IVar "fl") [1..]
        -- varsr = map (RT.Var . J.IVar "fr") [1..]
        varsr = repeat (RT.Fun (J.IConst 0) [])

    -- the lhs of ITS is linear; so we add additional constraints
    -- f(x,0,x) -> f(Y) | C == f(x,fresh1,fresh2) -> f(Y) | C && fresh1 == 0 && fresh2 == x
    linearise (RT.Rule (RT.Fun f as) r, cs) = (RT.Rule (RT.Fun f as') r, cs')
      where
        (as',cs',_) = foldr k ([],cs,0) as
        k a (asx,csx,i) = case a of
          c@(RT.Fun (J.IConst _) []) -> let v = RT.Var (J.IVar "fresh" i) in (v:asx, J.eq v c : csx, succ i)
          c@(RT.Var _)               ->
            if c `elem` asx
              then let v = RT.Var (J.IVar "fresh" i) in (v:asx, J.eq v c : csx, succ i)
              else (c:asx, csx, succ i)
          c                          -> error $ "linearise: oh no" ++ show c
    linearise _ = error "oh no"

    -- make sure that rules range over same variables
    normalizevars (RT.Rule l r, cs) =
      let
        (ml,il,l') = norm (M.empty,0,l)
        (mr,ir,r') = norm (ml,il,r)
        (_,_,cs')  = norms (mr,ir,cs)
      in (RT.Rule l' r', cs')
      where
        norm (m,i,RT.Var v) = case v `M.lookup` m of
          Just v' -> (m,i,RT.Var v')
          Nothing -> (M.insert v v' m, i+1, RT.Var v')
            where v' = J.IVar "" i
        norm (m,i,RT.Fun f ts) = (m', i', RT.Fun f ts') where
          (m',i',ts') = norms (m,i,ts)
        norms (m,i,ts) = foldr k (m,i,[]) ts
          where k t (n,j,ss) = let (n',j',t') = norm (n,j,t) in (n',j',t':ss)

