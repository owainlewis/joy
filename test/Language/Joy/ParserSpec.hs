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

      it "parses strings correctly" $ do
        readJoyExpr "\"hello\"" `shouldBe` literalResult (String "hello")
        readJoyExpr "\"\"" `shouldBe` literalResult (String "")

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
        readJoyExpr "[1 2 3]" `shouldBe` listResult [
          Literal (Integer 1),
          Literal (Integer 2),
          Literal (Integer 3)
        ]

        readJoyExpr "[true false]" `shouldBe` listResult [
          Literal (Boolean True),
          Literal (Boolean False)
        ]

      it "parses nested lists correctly" $ do
        readJoyExpr "[[1 2] [3 4]]" `shouldBe` listResult [
          List [Literal (Integer 1), Literal (Integer 2)],
          List [Literal (Integer 3), Literal (Integer 4)]
        ]

      it "parses lists with operators" $ do
        readJoyExpr "[+ - *]" `shouldBe` listResult [
          Literal (Identifier "+"),
          Literal (Identifier "-"),
          Literal (Identifier "*")
        ]

    describe "expressions" $ do
      it "parses simple arithmetic expressions" $ do
        readJoyExpr "1 2 +" `shouldBe` Right [
          Literal (Integer 1),
          Literal (Integer 2),
          Literal (Identifier "+")
        ]

      it "parses complex expressions" $ do
        readJoyExpr "[1 2 3] [dup *] map" `shouldBe` Right [
          List [Literal (Integer 1), Literal (Integer 2), Literal (Integer 3)],
          List [Literal (Identifier "dup"), Literal (Identifier "*")],
          Literal (Identifier "map")
        ]

      it "parses conditionals" $ do
        readJoyExpr "5 [0 =] [1] [dup pred] ifte" `shouldBe` Right [
          Literal (Integer 5),
          List [Literal (Integer 0), Literal (Identifier "=")],
          List [Literal (Integer 1)],
          List [Literal (Identifier "dup"), Literal (Identifier "pred")],
          Literal (Identifier "ifte")
        ]

    describe "definitions" $ do
      it "parses simple definitions correctly" $ do
        readJoyExpr "foo == 1 2 +" `shouldBe` Right [
          Definition "foo" [
            Literal (Integer 1),
            Literal (Integer 2),
            Literal (Identifier "+")
          ]
        ]

      it "parses definition lists correctly" $ do
        readJoyExpr "DEFINE foo == 1 2 + ; bar == 3 4 + ." `shouldBe` Right [
          DefinitionList [
            Definition "foo" [
              Literal (Integer 1),
              Literal (Integer 2),
              Literal (Identifier "+")
            ],
            Definition "bar" [
              Literal (Integer 3),
              Literal (Integer 4),
              Literal (Identifier "+")
            ]
          ]
        ]

      it "parses definitions with quotations" $ do
        readJoyExpr "square == [dup *]" `shouldBe` Right [
          Definition "square" [
            List [Literal (Identifier "dup"), Literal (Identifier "*")]
          ]
        ]

    describe "comments" $ do
      it "ignores comments" $ do
        readJoyExpr "1 2 + # this is a comment" `shouldBe` Right [
          Literal (Integer 1),
          Literal (Integer 2),
          Literal (Identifier "+")
        ]
