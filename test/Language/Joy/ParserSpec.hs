module Language.Joy.ParserSpec (spec) where

import           Language.Joy.AST
import           Language.Joy.Parser

import           Test.Hspec

spec :: Spec
spec =
  let literalResult x = Right [Literal x] in do
  describe "parseInteger" $ do
    it "will parse an integer correctly" $ do
      readExpr "10" `shouldBe` (literalResult (Integer 10))

