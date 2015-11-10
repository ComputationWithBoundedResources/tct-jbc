module Tct.Jbc.Data.Problem
  ( Jbc (..)
  , fromString
  , JbcStrategy
  , JbcDeclaration

  , CTrs (..)
  ) where


import qualified Jat.CompGraph                      as J (JGraph)
import qualified Jat.PState                         as J (PairSharing, SignedIntDomain)
import qualified Jat.Utils.TRS                      as J (PARule, prettyCTRS)
import qualified Jinja.Program                      as J (ClassId, MethodId, Program)
import qualified Jinja.Program.Data                 as J (initP)
import qualified Jinja.Program.Parser               as J (fromString)

import qualified Tct.Core                           as T
import qualified Tct.Core.Common.Pretty             as PP
import qualified Tct.Core.Common.Xml                as Xml


--- * jinja ----------------------------------------------------------------------------------------------------------
-- j2j lib needs alex and happy  :(

-- newtype Jinja = Jinja (JJ.MapOfMapClasses (),[(String,[String])])

-- instance Show Jinja      where show (Jinja (m,b)) = JJ.prettySourceCode b m
-- instance PP.Pretty Jinja where pretty             = PP.text . show
-- instance Xml.Xml Jinja   where toXml _            = Xml.empty

-- toJinja :: String -> Jinja
-- toJinja = Jinja . JJ.processClasses


--- * jbc ------------------------------------------------------------------------------------------------------------

data Jbc = Jbc
  { program :: J.Program
  , entry   :: Maybe (J.ClassId, J.MethodId) } -- when Nothing we assume there is a main function
  deriving Show

instance PP.Pretty Jbc where pretty p = PP.text . show $ program p
instance Xml.Xml Jbc   where toXml _  = Xml.empty

-- toJbc' :: Jinja -> Jbc
-- toJbc' (Jinja (m,b)) = Jbc . J.initP . J.fromString $ JJ.prettyJBC b (JJ.compileClasses m)

fromString :: String -> Jbc
fromString = flip Jbc Nothing . J.initP . J.fromString

type JbcStrategy    = T.Strategy Jbc Jbc
type JbcDeclaration = T.StrategyDeclaration Jbc Jbc


--- * cTRS -----------------------------------------------------------------------------------------------------------

-- CTRSs: term rewriting + Peano Arithmetic
-- defined function symbols occur only at root position
-- for a rule f(t1,...,tn) -> g(t1',...,tn') C, we call t1,...,tn and t1',...,tn' argument positions

data CTrs = CTrs (J.JGraph J.SignedIntDomain J.PairSharing) [J.PARule] deriving Show

instance PP.Pretty CTrs where pretty (CTrs _ rs) = J.prettyCTRS rs
instance Xml.Xml CTrs   where toXml _            = Xml.empty

