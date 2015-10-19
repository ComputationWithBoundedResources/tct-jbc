module Tct.Jbc.Config
  ( JbcConfig
  , jbcConfig
  , runJbc
  ) where


import qualified Control.Applicative        as A (optional)

import qualified Jinja.Program              as J (ClassId (..), MethodId (..))

import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Data              (deflFun)
import           Tct.Core.Main
import           Tct.Core

import           Tct.Jbc.Data.Problem
import           Tct.Jbc.Processor


type JbcConfig = TctConfig Jbc

runJbc :: JbcConfig -> IO ()
runJbc = tct3WithOptions jbcUpdate jbcOptions

jbcConfig :: TctConfig Jbc
jbcConfig = defaultTctConfig parserIO
  `withDefaultStrategy` deflFun jbcDeclaration
  `addStrategies` jbcDeclarations

parserIO :: FilePath -> IO (Either String Jbc)
parserIO = fmap parser . readFile

parser :: String -> Either String Jbc
parser = Right . fromString

newtype JbcOptions =  JbcOptions (Maybe (String,String))



jbcOptions :: Options JbcOptions
jbcOptions = JbcOptions
  <$> A.optional (option' readEntry
    (eopt
    `withArgLong` "entry"
    `withCropped` 'e'
    `withHelpDoc` PP.text "CLASS methodname"))
  where readEntry s = return (takeWhile (/= ' ') s, tail $ dropWhile (/= ' ') s)

jbcUpdate :: JbcConfig -> JbcOptions -> JbcConfig
jbcUpdate cfg (JbcOptions Nothing)        = cfg
jbcUpdate cfg (JbcOptions (Just (cn,mn))) = cfg
  { parseProblem = \fp -> fmap setEntry  <$> parseProblem cfg fp }
    where setEntry jbc = jbc {entry = Just (J.ClassId cn, J.MethodId mn) }

