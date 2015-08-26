module Tct.Jbc.Data.Mode
  ( JbcMode
  , jbcMode
  , JbcOptions
  ) where


import           Control.Applicative

import qualified Jinja.Program              as J (ClassId (..), MethodId (..))

import qualified Tct.Core.Common.Pretty     as PP
import           Tct.Core.Main
import           Tct.Core.Processor.Failing (failing)

import           Tct.Jbc.Data.Problem


type JbcMode = TctMode Jbc Jbc JbcOptions

jbcMode :: JbcMode
jbcMode = TctMode
  { modeId              = "jbc"
  , modeParser          = parserIO
  , modeStrategies      = []

  , modeDefaultStrategy = failing
  , modeOptions         = options
  , modeModifyer        = modifyer
  , modeAnswer          = \_ _  -> return ()
  , modeProof           = \_ _ -> return () }

parserIO :: FilePath -> IO (Either String Jbc)
parserIO = fmap parser . readFile

parser :: String -> Either String Jbc
parser = Right . fromString

newtype JbcOptions =  JbcOptions (Maybe (String,String))

options :: Options JbcOptions
options = JbcOptions
  <$> optional (option $ eopt
    `withArgLong` "entry"
    `withCropped` 'e'
    `withHelpDoc` PP.text "The entry point.")

modifyer :: JbcOptions -> Jbc -> Jbc
modifyer (JbcOptions Nothing) jbc        = jbc
modifyer (JbcOptions (Just (cn,mn))) jbc = jbc { entry = Just (J.ClassId cn,J.MethodId mn)}

