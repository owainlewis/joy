module Language.Joy.ParserSpec (spec) where

import           Language.Joy.AST
import           Language.Joy.Parser

import           Test.Hspec

spec :: Spec
spec =
  let literalResult x = Right [Literal x]
      listResult xs = Right [List xs] in do
  describe "parser" $ do
    describe "literals" $ do
      it "parses integers correctly" $ do
        readJoyExpr "10" `shouldBe` literalResult (Integer 10)
        readJoyExpr "-42" `shouldBe` literalResult (Integer (-42))

      it "parses floats correctly" $ do
        readJoyExpr "3.14" `shouldBe` literalResult (Float 3.14)
        readJoyExpr "-0.5" `shouldBe` literalResult (Float (-0.5))

      it "parses booleans correctly" $ do
        readJoyExpr "true" `shouldBe` literalResult (Boolean True)
        readJoyExpr "false" `shouldBe` literalResult (Boolean False)

      it "does not split identifiers that start with boolean text" $ do
        readJoyExpr "true-value" `shouldBe` literalResult (Identifier "true-value")
        readJoyExpr "falsehood" `shouldBe` literalResult (Identifier "falsehood")

      it "parses strings correctly" $ do
        readJoyExpr "\"hello\"" `shouldBe` literalResult (String "hello")
        readJoyExpr "\"\"" `shouldBe` literalResult (String "")

      it "parses quoted characters correctly" $ do
        readJoyExpr "'a'" `shouldBe` literalResult (Char 'a')
        readJoyExpr "'a" `shouldSatisfy` either (const True) (const False)

      it "parses word identifiers correctly" $ do
        readJoyExpr "dup" `shouldBe` literalResult (Identifier "dup")
        readJoyExpr "swap" `shouldBe` literalResult (Identifier "swap")
        readJoyExpr "my-func" `shouldBe` literalResult (Identifier "my-func")
        readJoyExpr "integer?" `shouldBe` literalResult (Identifier "integer?")

      it "parses symbolic operators correctly" $ do
        readJoyExpr "+" `shouldBe` literalResult (Identifier "+")
        readJoyExpr "-" `shouldBe` literalResult (Identifier "-")
        readJoyExpr "*" `shouldBe` literalResult (Identifier "*")
        readJoyExpr "/" `shouldBe` literalResult (Identifier "/")
        readJoyExpr "<" `shouldBe` literalResult (Identifier "<")
        readJoyExpr ">" `shouldBe` literalResult (Identifier ">")
        readJoyExpr "<=" `shouldBe` literalResult (Identifier "<=")
        readJoyExpr ">=" `shouldBe` literalResult (Identifier ">=")
        readJoyExpr "=" `shouldBe` literalResult (Identifier "=")
        readJoyExpr "!=" `shouldBe` literalResult (Identifier "!=")

    describe "lists" $ do
      it "parses empty lists correctly" $ do
        readJoyExpr "[]" `shouldBe` listResult []

      it "parses lists with elements correctly" $ do
        let numbers =
              [ Literal (Integer 1)
              , Literal (Integer 2)
              , Literal (Integer 3)
              ]
            booleans =
              [ Literal (Boolean True)
              , Literal (Boolean False)
              ]
        readJoyExpr "[1 2 3]" `shouldBe` listResult numbers
        readJoyExpr "[true false]" `shouldBe` listResult booleans

      it "parses nested lists correctly" $ do
        let nested =
              [ List [Literal (Integer 1), Literal (Integer 2)]
              , List [Literal (Integer 3), Literal (Integer 4)]
              ]
        readJoyExpr "[[1 2] [3 4]]" `shouldBe` listResult nested

      it "parses lists with operators" $ do
        let operators =
              [ Literal (Identifier "+")
              , Literal (Identifier "-")
              , Literal (Identifier "*")
              ]
        readJoyExpr "[+ - *]" `shouldBe` listResult operators

    describe "expressions" $ do
      it "parses simple arithmetic expressions" $ do
        let expression =
              [ Literal (Integer 1)
              , Literal (Integer 2)
              , Literal (Identifier "+")
              ]
        readJoyExpr "1 2 +" `shouldBe` Right expression

      it "parses complex expressions" $ do
        let expression =
              [ List [Literal (Integer 1), Literal (Integer 2), Literal (Integer 3)]
              , List [Literal (Identifier "dup"), Literal (Identifier "*")]
              , Literal (Identifier "map")
              ]
        readJoyExpr "[1 2 3] [dup *] map" `shouldBe` Right expression

      it "parses conditionals" $ do
        let expression =
              [ Literal (Integer 5)
              , List [Literal (Integer 0), Literal (Identifier "=")]
              , List [Literal (Integer 1)]
              , List [Literal (Identifier "dup"), Literal (Identifier "pred")]
              , Literal (Identifier "ifte")
              ]
        readJoyExpr "5 [0 =] [1] [dup pred] ifte" `shouldBe` Right expression

    describe "definitions" $ do
      it "parses simple definitions correctly" $ do
        let definition =
              Definition "foo"
                [ Literal (Integer 1)
                , Literal (Integer 2)
                , Literal (Identifier "+")
                ]
        readJoyExpr "foo == 1 2 +" `shouldBe` Right [definition]

      it "parses definitions with word-like names" $ do
        let definition =
              Definition "square-number?"
                [ Literal (Identifier "dup")
                , Literal (Identifier "*")
                ]
        readJoyExpr "square-number? == dup *" `shouldBe` Right [definition]

      it "parses definition lists correctly" $ do
        let definitions =
              DefinitionList
                [ Definition "foo"
                    [ Literal (Integer 1)
                    , Literal (Integer 2)
                    , Literal (Identifier "+")
                    ]
                , Definition "bar"
                    [ Literal (Integer 3)
                    , Literal (Integer 4)
                    , Literal (Identifier "+")
                    ]
                ]
        readJoyExpr "DEFINE foo == 1 2 + ; bar == 3 4 + ." `shouldBe` Right [definitions]

      it "parses definitions with quotations" $ do
        let definition =
              Definition "square"
                [List [Literal (Identifier "dup"), Literal (Identifier "*")]]
        readJoyExpr "square == [dup *]" `shouldBe` Right [definition]

    describe "comments" $ do
      it "ignores comments" $ do
        let expression =
              [ Literal (Integer 1)
              , Literal (Integer 2)
              , Literal (Identifier "+")
              ]
        readJoyExpr "1 2 + # this is a comment" `shouldBe` Right expression
