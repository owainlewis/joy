module Main (main) where

import qualified Language.Joy.IntegrationSpec as IntegrationSpec
import qualified Language.Joy.ParserSpec as ParserSpec
import qualified Language.Joy.VMSpec as VMSpec
import           Test.Hspec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  VMSpec.spec
  IntegrationSpec.spec
