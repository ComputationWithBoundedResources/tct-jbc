{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
-- | This module provides (realtive) polynomial interpretations for ctrs.
module Tct.Jbc.Processor.PolynomialInterpretation
  ( polys
  , toCTrs'
  , dropWeakSuffix
  ) where

-- import Debug.Trace

import Data.Monoid ((<>))
import qualified Data.Map.Strict                     as M
import           Data.Maybe                          (fromMaybe)
import qualified Data.Set                            as S

import qualified Data.Rewriting.Rule                 as R
import qualified Data.Rewriting.Term                 as T

import qualified Tct.Common.Polynomial               as P
import qualified Tct.Common.PolynomialInterpretation as PI
import           Tct.Common.Ring
import           Tct.Common.SMT                      ((.&&), (.==), (.=>), (.>), (.>=))
import qualified Tct.Common.SMT                      as SMT


import qualified Jat.CompGraph                       as J (JGraph)
import qualified Jat.Constraints                     as J
import qualified Jat.Utils.TRS                       as J

import           Tct.Core
import qualified Tct.Core.Common.Pretty              as PP
import qualified Tct.Core.Common.Xml                 as Xml
import qualified Tct.Core.Data                       as T
import qualified Tct.Core.Processor.Empty            as T (empty)
import           Tct.Core.Processor.Transform        (transform)

import qualified Data.Rewriting.Rule                 as RT hiding (vars)

import           Tct.Jbc.Data.Problem


-- MS: TODO: from here on
-- transform to TRS and/or ITS
-- under which conditions can I introduce bounds in the interpretation
-- f(S(x)) -> f(x) ; here I want x >= 0; dedicated ArgFilter ...
-- Argfilter/Projection is sound; that has to be used?
-- not necessary for int part as 
-- make combined argfilter + rank over natural domain


--- * relative ctrs --------------------------------------------------------------------------------------------------

-- | Problem representation for (relative) constraint rewriting..
data CTrs' = CTrs' { sRules :: [J.PARule] , wRules :: [J.PARule] } deriving Show

signature :: CTrs' -> M.Map J.PAFun Int
signature (CTrs' srs wrs) = M.fromList $ foldr k [] (srs ++ wrs)
  where k (RT.Rule lhs rhs ,cs) = T.funsDL (T.withArity lhs) <> T.funsDL (T.withArity rhs) <> foldr (mappend . T.funsDL . T.withArity) id cs

symbols :: CTrs' -> S.Set J.PAFun
symbols = M.keysSet . signature

defineds :: CTrs' -> S.Set J.PAFun
defineds (CTrs' srs wrs) = foldr k S.empty srs <> foldr k S.empty wrs
  where
    k (RT.Rule (T.Fun f _) _, _) = S.insert f
    k _                          = id

constructors :: CTrs' -> S.Set J.PAFun
constructors ctrs = symbols ctrs `S.difference` defineds ctrs


--- * polynomial interpretation --------------------------------------------------------------------------------------

data PolynomialInterpretation = PolynomialInterpretation { shape :: PI.Shape } deriving Show

data Coefficient  = SomeCoefficient | RestrictCoefficient | IntCoefficient Int
  deriving (Eq, Ord, Show)

newtype Indeterminate = Indeterminate Int deriving (Eq, Ord, Show, Enum)

instance PP.Pretty Indeterminate where
  pretty i = PP.text "x_" <> PP.int (fromEnum i)

indeterminates :: [Indeterminate]
indeterminates = [Indeterminate 1..]

type Poly f v = P.Polynomial f v
type PolyV    = P.PView Coefficient Indeterminate

-- TODO: MS generalise type in Tct.Common.PolynomialInterpretation
newtype Interpretation f a = Interpretation { interpretations :: M.Map f a }
  deriving (Show, Functor, Foldable, Traversable)

instance (PP.Pretty f, PP.Pretty a) => PP.Pretty (Interpretation f a) where
  pretty pint = PP.table [(PP.AlignRight, as), (PP.AlignLeft, bs), (PP.AlignLeft,cs)]
    where
      (as,bs,cs) = unzip3 $ map k (M.toList $ interpretations pint)
      k (f,p)    = (PP.text "p" <> PP.parens (PP.pretty f), PP.text " = ", PP.pretty p)


instance (SMT.Decode m c a, Additive a, Eq a, Additive c, Eq c) => SMT.Decode m (Poly c v) (Poly a v) where
  decode = P.mapCoefficientsM SMT.decode

instance (SMT.Decode m c a) => SMT.Decode m (Interpretation fun c) (Interpretation fun a) where
  decode (Interpretation m) = Interpretation <$> traverse SMT.decode m


fixedInterpretation :: M.Map J.PAFun PolyV
fixedInterpretation = M.fromList
  [(J.IConst 1, [(IntCoefficient 1,[])])
  ,(J.Add     , [(IntCoefficient 1, [(Indeterminate 1,1)]), (IntCoefficient 1   , [(Indeterminate 2,1)])])
  ,(J.Sub     , [(IntCoefficient 1, [(Indeterminate 1,1)]), (IntCoefficient (-1), [(Indeterminate 2,1)])])]

mkInterpretation :: PI.Shape -> S.Set J.PAFun -> (J.PAFun, Int) -> PolyV
mkInterpretation shp cs (f, ar) = case f of
  J.IConst i -> [(IntCoefficient i,[])]
  -- J.Add      -> [(IntCoefficient 1, [(Indeterminate 1,1)]), (IntCoefficient 1   , [(Indeterminate 2,1)])]
  -- J.Sub      -> [(IntCoefficient 1, [(Indeterminate 1,1)]), (IntCoefficient (-1), [(Indeterminate 2,1)])]
  _ | f `elem` cs ->  P.linear (const RestrictCoefficient) vs
    | otherwise   ->  fromShape shp (const mkCoefficient) vs
  where
    vs            = take ar indeterminates
    mkCoefficient = if shp == PI.StronglyLinear then RestrictCoefficient else SomeCoefficient
    fromShape PI.StronglyLinear = P.linear
    fromShape PI.Linear         = P.linear
    fromShape PI.Quadratic      = P.quadratic
    fromShape (PI.Mixed i)      = P.mixed i

toGte :: [J.PATerm] ->  [J.PATerm]
toGte = foldr k []
  where
    k (T.Fun J.Lt [t2,t1])  = (T.Fun J.Sub [t1, T.Fun J.Add [t2, T.Fun (J.IConst 1) [] ]] :)
    k (T.Fun J.Lte [t2,t1]) = (T.Fun J.Sub [t1,t2] :)
    k (T.Fun J.Gte [t1,t2]) = (T.Fun J.Sub [t1,t2] :)
    k (T.Fun J.Gt [t1,t2])  = (T.Fun J.Sub [t1, T.Fun J.Add [t2, T.Fun (J.IConst 1) [] ]] :)
    k _                     = id

data Order c1 c2 = Order J.PARule c1 (Poly c2 J.PAVar) (Poly c2 J.PAVar)
  deriving Show

data PolyOrder = PolyOrder
  { pint_   :: Interpretation J.PAFun (Poly Int Indeterminate)
  , bound_  :: T.Complexity
  , srs_    :: [Order Int Int]
  , wrs_    :: [Order Int Int]
  -- interpretation
  , strict_ :: [Order Int Int]
  , weak_   :: [Order Int Int]
  } deriving Show

out_ :: PolyOrder -> CTrs'
out_ proof = CTrs' srs (wrs ++ wrs')
  where
    (srs,wrs') = foldr (\(Order r i _ _) (ss,ws) -> if i > 0 then (ss,r:ws) else (r:ss,ws)) ([],[]) (srs_ proof)
    wrs        = foldr (\(Order r _ _ _) ws -> r:ws) [] (wrs_ proof)

-- TODO: MS: should not be here
instance PP.Pretty J.PAVar where pretty = J.prettyPATerm . T.Var

-- TODO: MS: should not be here
instance PP.Pretty J.PAFun where pretty = PP.text . show

instance PP.Pretty PolyOrder where
  pretty order = PP.vcat
    [ PP.text "We apply following polynomial interpretation:"
    , PP.indent 2 (PP.pretty (pint_ order))
    , PP.text ""
    , PP.text "Following rules are strictly oriented:"
    , ppOrder (PP.text "   > ") (strict_ order)
    , PP.text ""
    , PP.text "Following rules are weakly oriented:"
    , ppOrder (PP.text "  >= ") (weak_ order) ]
    where
      ppOrder cmp rs = PP.table [(PP.AlignRight, as), (PP.AlignLeft, bs), (PP.AlignLeft, cs)]
        where
          (as,bs,cs) = unzip3 $ concatMap ppRule rs
          ppRule (Order (RT.Rule lhs rhs, cs) i instlhs instrhs) =
            [ (PP.list (fmap J.prettyPATerm cs)                , PP.text " ==> " , PP.empty)
            , (PP.indent 2 (J.prettyPATerm  lhs) , PP.text "   = " , PP.pretty instlhs)
            , (PP.empty                    , cmp             , PP.pretty instrhs )
            , (PP.empty                    , PP.text "   = " , J.prettyPATerm rhs)
            , (PP.empty                    , PP.empty        , PP.empty) ]


instance Xml.Xml PolyOrder where
  toXml = const Xml.empty

instance (SMT.Decode m c1 a1, SMT.Decode m c2 a2, Eq c2, Ord a2, Additive c2, Additive a2) => SMT.Decode m (Order c1 c2) (Order a1 a2) where
  decode (Order r s lint rint) = Order r <$> SMT.decode s <*> SMT.decode lint <*> SMT.decode rint


entscheide :: PolynomialInterpretation -> CTrs' -> T.TctM (SMT.Result PolyOrder)
-- entscheide npi ctrs | trace (show $ PP.pretty ctrs) False = undefined
entscheide npi ctrs = fmap k `fmap` entscheide' npi ctrs
  where
    k (pint, srs, wrs) =
      let (ss,ws) = foldr (\o@(Order r i _ _) (ss',ws') -> if i > 0 then (o:ss',ws') else (ss',o:ws')) ([],[]) srs in
      PolyOrder
        { pint_   = pint
        , bound_  = fromShape (shape npi)
        , srs_    = srs
        , wrs_    = wrs
        , strict_ = ss
        , weak_   = wrs ++ ws }
    fromShape PI.StronglyLinear = T.linear
    fromShape PI.Linear         = T.linear
    fromShape PI.Quadratic      = T.Poly (Just 2)
    fromShape (PI.Mixed i)      = T.Poly (Just i)

entscheide' :: PolynomialInterpretation -> CTrs' -> T.TctM (SMT.Result (Interpretation J.PAFun (Poly Int Indeterminate), [Order Int Int], [Order Int Int]))
entscheide' npi ctrs = SMT.smtSolveSt solver $ do
  SMT.setLogic SMT.QF_NIA
  ebsi    <- Interpretation <$> mapM encode absi
  stricts <- M.fromList <$> mapM (\(i,_) -> SMT.nvarM' >>= \v -> return (i,v)) srules

  let
    interpret = interpretf ebsi
    strict    = find stricts
    interpretCon = fmap interpret . toGte
    absolute p = SMT.bigAnd [ c .== SMT.zero | c <- P.coefficients p ]
    eliminate p ps = do
      let weigh x = SMT.nvarM' >>= \lambda -> return (lambda `P.scale` x)
      ps2 <- mapM weigh ps
      let
        (p1,pc1) = P.splitConstantValue p
        (p2,pc2) = P.splitConstantValue (bigAdd ps2)
      return $ absolute (p1 `sub` p2) .&& (pc1 .>= pc2)

    sdecrease (i, (R.Rule lhs rhs, cs)) = pl `eliminate` interpretCon cs
      where pl = interpret lhs `sub` (interpret rhs `add` P.constant (strict i))
    sbounded (_, (R.Rule lhs _, cs)) = pl `eliminate` interpretCon cs
      where pl = interpret lhs
    wdecrease (R.Rule lhs rhs, cs) = pl `eliminate` interpretCon cs
      where pl = interpret lhs `sub` interpret rhs

    sorder (i,r) = do
      fm1 <- sdecrease (i,r)
      fm2 <- sbounded  (i,r)
      return $ fm1 .&& (strict i .> zero .=> fm2)

  SMT.assert (SMT.top :: SMT.Formula Int)
  SMT.assert =<< SMT.bigAndM [ sorder r | r <- srules ]
  SMT.assert =<< SMT.bigAndM [ wdecrease r | r <- wrules ]
  SMT.assert $ SMT.bigOr [ strict i .> zero | (i,_) <- srules ]

  return $ SMT.decode
    (ebsi
    , [ Order r (stricts `find` i)          (interpret lhs) (interpret rhs) | (i,r@(R.Rule lhs rhs, _)) <- srules ]
    , [ Order r (SMT.zero :: SMT.IExpr Int) (interpret lhs) (interpret rhs) | r@(R.Rule lhs rhs, _) <- wrules ] )

  where
    solver = SMT.minismt
    -- solver = SMT.z3
    srules = zip [(1::Int)..] (sRules ctrs)
    wrules = wRules ctrs
    absi   = fixedInterpretation `M.union` M.mapWithKey (curry (mkInterpretation (shape npi) (constructors ctrs))) (signature ctrs)
    encode = P.fromViewWithM $ \c -> case c of
      SomeCoefficient     -> SMT.ivarM'
      RestrictCoefficient -> SMT.sivarM'
      IntCoefficient i    -> return $ SMT.num i


interpretTerm :: (J.PAFun -> [a] -> a) -> (J.PAVar -> a) -> J.PATerm -> a
interpretTerm fun var (T.Fun f ts) = fun f (interpretTerm fun var `fmap` ts)
interpretTerm _   var (T.Var v)    = var v

interpretf :: (Eq c, Multiplicative c, Additive c) => Interpretation J.PAFun (Poly c Indeterminate) -> J.PATerm -> Poly c J.PAVar
interpretf ebsi = interpretTerm interpretFun interpretVar
  where
    interpretFun f = P.substituteVariables (interpretations ebsi `find` f) . M.fromList . zip indeterminates
    interpretVar   = P.variable

find :: (Ord k, Show k) => M.Map k a -> k -> a
find m k = error err `fromMaybe` M.lookup k m
  where err = "Tct.Jbc.Processor.PI: key " ++ show k ++ " not found."

-- apply toITs without filtering
fromCTrs :: CTrs -> CTrs'
fromCTrs (CTrs gr rs) = CTrs' (toc gr rs) []

toc :: J.JGraph i a -> [J.PARule] -> [J.PARule]
toc gr =
  S.toList
  . S.fromList
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


--- * tct integration ------------------------------------------------------------------------------------------------

instance PP.Pretty CTrs' where
  pretty (CTrs' srs wrs) = PP.vcat
    [ PP.text "Strict Rules:"
    , PP.indent 2 $ J.prettyCTRS srs
    , PP.text "Weak Rules:"
    , PP.indent 2 $ J.prettyCTRS wrs ]

instance Xml.Xml CTrs' where
  toXml _ = Xml.empty

toCTrs' :: Strategy CTrs CTrs'
toCTrs' = transform "We simplify the original CTrs' problem." $ Right . fromCTrs

instance T.Processor PolynomialInterpretation where
  type ProofObject PolynomialInterpretation = PolyOrder
  type In  PolynomialInterpretation         = CTrs'
  type Out PolynomialInterpretation         = CTrs'

  execute proc prob = do
    res <- entscheide proc prob
    case res of
      SMT.Sat proof -> T.succeedWith1 proof (\(T.Id c) -> T.updateTimeUBCert c (`add` bound_ proof)) (out_ proof)
      _             -> T.abortWith "Incompatible"


empty :: Strategy CTrs' CTrs'
empty = T.empty (null . sRules)

constant, linear, stronglyLinear, quadratic :: Strategy CTrs' CTrs'
constant       = T.processor $ PolynomialInterpretation (PI.Mixed 0)
linear         = T.processor $ PolynomialInterpretation PI.Linear
stronglyLinear = T.processor $ PolynomialInterpretation PI.StronglyLinear
quadratic      = T.processor $ PolynomialInterpretation PI.Quadratic
mixed :: Int -> Strategy CTrs' CTrs'
mixed i        = T.processor $ PolynomialInterpretation (PI.Mixed i)

pxs :: Int -> Strategy CTrs' CTrs'
pxs 0 = constant
pxs 1 = stronglyLinear .<||> linear
pxs 2 = quadratic .<||> mixed 2
pxs n = mixed (abs n)

polys :: Int -> Int -> Strategy CTrs' CTrs'
polys l u = chain [ te $ (pxs n .>>> try dropWeakSuffix) .<||> empty | n <- [max 0 (min l u)..max 0 u] ]

dropWeakSuffix' :: CTrs' -> Either String CTrs'
dropWeakSuffix' ctrs
  | length wrs' >= length (wRules ctrs) = Left "No weak suffixes."
  | otherwise    = Right $ ctrs { wRules = wrs' }
  where
    wrs' = filter (\wr -> any (k wr) (sRules ctrs)) (wRules ctrs)
    k (R.Rule _ rhs,_) (R.Rule lhs _, _) = T.root rhs == T.root lhs

dropWeakSuffix :: Strategy CTrs' CTrs'
dropWeakSuffix = transform "We drop weak suffixes." dropWeakSuffix'

