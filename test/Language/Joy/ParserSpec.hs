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
        readJoyExpr "10" `shouldBe` (literalResult (Integer 10))
        readJoyExpr "-42" `shouldBe` (literalResult (Integer (-42)))
      
      it "parses floats correctly" $ do
        readJoyExpr "3.14" `shouldBe` (literalResult (Float 3.14))
        readJoyExpr "-0.5" `shouldBe` (literalResult (Float (-0.5)))
      
      it "parses booleans correctly" $ do
        readJoyExpr "true" `shouldBe` (literalResult (Boolean True))
        readJoyExpr "false" `shouldBe` (literalResult (Boolean False))
      
      it "parses strings correctly" $ do
        readJoyExpr "\"hello\"" `shouldBe` (literalResult (String "hello"))
        readJoyExpr "\"\"" `shouldBe` (literalResult (String ""))
      
      it "parses identifiers correctly" $ do
        readJoyExpr "dup" `shouldBe` (literalResult (Identifier "dup"))
        readJoyExpr "swap" `shouldBe` (literalResult (Identifier "swap"))
    
    describe "lists" $ do
      it "parses empty lists correctly" $ do
        readJoyExpr "[]" `shouldBe` (listResult [])
      
      it "parses lists with elements correctly" $ do
        readJoyExpr "[1 2 3]" `shouldBe` (listResult [
          Literal (Integer 1),
          Literal (Integer 2),
          Literal (Integer 3)
        ])
        
        readJoyExpr "[true false]" `shouldBe` (listResult [
          Literal (Boolean True),
          Literal (Boolean False)
        ])
      
      it "parses nested lists correctly" $ do
        readJoyExpr "[[1 2] [3 4]]" `shouldBe` (listResult [
          List [Literal (Integer 1), Literal (Integer 2)],
          List [Literal (Integer 3), Literal (Integer 4)]
        ])
    
    describe "definitions" $ do
      it "parses simple definitions correctly" $ do
        readJoyExpr "foo == 1 2 +" `shouldBe` (Right [
          Definition "foo" [
            Literal (Integer 1),
            Literal (Integer 2),
            Literal (Identifier "+")
          ]
        ])
      
      it "parses definition lists correctly" $ do
        readJoyExpr "DEFINE foo == 1 2 + ; bar == 3 4 + ." `shouldBe` (Right [
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
        ])

