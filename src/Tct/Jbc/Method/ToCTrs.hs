module Tct.Jbc.Method.ToCTrs (toCTrs) where


import           Control.Monad.Identity (runIdentity)

import qualified Jat.CompGraph          as J (MkJGraph, mkJGraph, mkJGraph2TRS)
import qualified Jat.JatM               as J (Jat, evalJat, initJat)
import qualified Jat.PState             as J (PairSharing, SignedIntDomain)
import qualified Jinja.Program          as J (MethodId (..), methods)

import qualified Tct.Core.Combinators   as T
import qualified Tct.Core.Data          as T

import           Tct.Jbc.Data


toCTrs :: T.Strategy Jbc CTrs
toCTrs = T.transform (T.Continue . toCTrs')

toCTrs' :: Jbc -> CTrs
toCTrs' (Jbc p eM) = runIdentity $ flip J.evalJat (J.initJat p) $ do
  gr <- J.mkJGraph cn mn :: J.Jat (J.MkJGraph J.SignedIntDomain J.PairSharing)
  tr <- J.mkJGraph2TRS gr
  return $ uncurry CTrs tr
  where
    (cn,mn) = head [ d | d <- J.methods p, guard eM d ]
    guard (Just d1) d2    = d1 == d2
    guard Nothing (_,mn') = mn' == J.MethodId "main"

