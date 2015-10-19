module Tct.Jbc.Method.ToCTrs (toCTrs) where


import           Control.Monad.Identity (runIdentity)

import qualified Jat.CompGraph          as J (MkJGraph, mkJGraph, mkJGraph2TRS)
import qualified Jat.JatM               as J (Jat, evalJat, initJat)
import qualified Jat.PState             as J (PairSharing, SignedIntDomain)
import qualified Jinja.Program          as J (MethodId (..), methods)

import qualified Tct.Core.Data          as T
import           Tct.Core.Processor.Transform (transform)

import           Tct.Jbc.Data


toCTrs :: T.Strategy Jbc CTrs
toCTrs = transform "We abstract bytecode to cTRS" toCTrs'

toCTrs' :: Jbc -> Either String CTrs
toCTrs' (Jbc p eM) = do
  let es = [ d | d <- J.methods p, guard eM d ]
  (cn,mn) <- 
      if null es
        then Left "Main not found; specify program entry" 
        else Right (head es)
  return $
    runIdentity $ flip J.evalJat (J.initJat p) $ do
    gr <- J.mkJGraph cn mn :: J.Jat (J.MkJGraph J.SignedIntDomain J.PairSharing)
    tr <- J.mkJGraph2TRS gr
    return $ uncurry CTrs tr
  where
    guard (Just d1) d2    = d1 == d2
    guard Nothing (_,mn') = mn' == J.MethodId "main"

