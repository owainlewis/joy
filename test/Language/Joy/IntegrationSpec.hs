{-# LANGUAGE OverloadedStrings #-}
module Language.Joy.IntegrationSpec (spec) where

import           Language.Joy
import           Language.Joy.VirtualMachine (Joy(..))
import           Test.Hspec

-- Helper to test source code evaluation
shouldRunTo :: String -> [Joy] -> Expectation
shouldRunTo source expected =
  case runJoy source of
    Left err -> expectationFailure $ "Error: " ++ err
    Right stack -> stack `shouldBe` expected

-- Helper to test that source code fails
shouldFail :: String -> Expectation
shouldFail source =
  case runJoy source of
    Left _ -> return ()
    Right stack -> expectationFailure $ "Expected error but got: " ++ show stack

spec :: Spec
spec = do
  describe "Integration Tests" $ do
    ---------------------------------------------------------------------
    -- Basic Arithmetic
    ---------------------------------------------------------------------
    describe "arithmetic" $ do
      it "evaluates simple addition" $ do
        "1 2 +" `shouldRunTo` [JInt 3]

      it "evaluates chained arithmetic" $ do
        "1 2 + 3 +" `shouldRunTo` [JInt 6]

      it "evaluates multiplication" $ do
        "3 4 *" `shouldRunTo` [JInt 12]

      it "evaluates division" $ do
        "10 4 /" `shouldRunTo` [JFloat 2.5]

      it "evaluates complex expressions" $ do
        "2 3 + 4 *" `shouldRunTo` [JInt 20]

      it "evaluates with negative numbers" $ do
        "-5 3 +" `shouldRunTo` [JInt (-2)]

    ---------------------------------------------------------------------
    -- Stack Operations
    ---------------------------------------------------------------------
    describe "stack operations" $ do
      it "dup duplicates" $ do
        "5 dup" `shouldRunTo` [JInt 5, JInt 5]

      it "swap swaps" $ do
        "1 2 swap" `shouldRunTo` [JInt 1, JInt 2]

      it "pop removes" $ do
        "1 2 pop" `shouldRunTo` [JInt 1]

      it "dup *" $ do
        "5 dup *" `shouldRunTo` [JInt 25]

    ---------------------------------------------------------------------
    -- Lists and Quotations
    ---------------------------------------------------------------------
    describe "lists" $ do
      it "creates lists" $ do
        "[1 2 3]" `shouldRunTo` [JQuote [JInt 1, JInt 2, JInt 3]]

      it "nested lists" $ do
        "[[1 2] [3 4]]" `shouldRunTo`
          [JQuote [JQuote [JInt 1, JInt 2], JQuote [JInt 3, JInt 4]]]

      it "cons prepends" $ do
        "1 [2 3] cons" `shouldRunTo` [JQuote [JInt 1, JInt 2, JInt 3]]

      it "first extracts" $ do
        "[1 2 3] first" `shouldRunTo` [JInt 1]

      it "rest extracts" $ do
        "[1 2 3] rest" `shouldRunTo` [JQuote [JInt 2, JInt 3]]

    ---------------------------------------------------------------------
    -- Quotation Execution
    ---------------------------------------------------------------------
    describe "quotation execution" $ do
      it "i executes a quotation" $ do
        "5 [dup *] i" `shouldRunTo` [JInt 25]

      it "i with arithmetic" $ do
        "3 4 [+] i" `shouldRunTo` [JInt 7]

      it "dip executes under" $ do
        "1 2 [10 +] dip" `shouldRunTo` [JInt 2, JInt 11]

      it "nested i" $ do
        "5 [[dup *] i] i" `shouldRunTo` [JInt 25]

    ---------------------------------------------------------------------
    -- Map, Filter, Fold
    ---------------------------------------------------------------------
    describe "higher-order combinators" $ do
      it "map squares" $ do
        "[1 2 3] [dup *] map" `shouldRunTo` [JQuote [JInt 1, JInt 4, JInt 9]]

      it "filter greater than 2" $ do
        "[1 2 3 4] [2 >] filter" `shouldRunTo` [JQuote [JInt 3, JInt 4]]

      it "fold sum" $ do
        "[1 2 3 4] 0 [+] fold" `shouldRunTo` [JInt 10]

      it "map then fold" $ do
        "[1 2 3] [dup *] map 0 [+] fold" `shouldRunTo` [JInt 14]

    ---------------------------------------------------------------------
    -- Conditionals
    ---------------------------------------------------------------------
    describe "conditionals" $ do
      it "ifte true branch" $ do
        "5 [3 >] [100] [0] ifte" `shouldRunTo` [JInt 100, JInt 5]

      it "ifte false branch" $ do
        "2 [3 >] [100] [0] ifte" `shouldRunTo` [JInt 0, JInt 2]

      it "choice true" $ do
        "true 1 2 choice" `shouldRunTo` [JInt 1]

      it "choice false" $ do
        "false 1 2 choice" `shouldRunTo` [JInt 2]

    ---------------------------------------------------------------------
    -- Boolean Operations
    ---------------------------------------------------------------------
    describe "booleans" $ do
      it "and" $ do
        "true true and" `shouldRunTo` [JBool True]
        "true false and" `shouldRunTo` [JBool False]

      it "or" $ do
        "false true or" `shouldRunTo` [JBool True]
        "false false or" `shouldRunTo` [JBool False]

      it "not" $ do
        "true not" `shouldRunTo` [JBool False]
        "false not" `shouldRunTo` [JBool True]

    ---------------------------------------------------------------------
    -- Comparisons
    ---------------------------------------------------------------------
    describe "comparisons" $ do
      it "less than" $ do
        "3 5 <" `shouldRunTo` [JBool True]
        "5 3 <" `shouldRunTo` [JBool False]

      it "equality" $ do
        "5 5 =" `shouldRunTo` [JBool True]
        "5 3 =" `shouldRunTo` [JBool False]

      it "greater than or equal" $ do
        "5 5 >=" `shouldRunTo` [JBool True]
        "6 5 >=" `shouldRunTo` [JBool True]

    ---------------------------------------------------------------------
    -- Recursion
    ---------------------------------------------------------------------
    describe "recursion" $ do
      it "factorial with linrec" $ do
        "5 [0 =] [pop 1] [dup 1 -] [*] linrec" `shouldRunTo` [JInt 120]

      it "sum list with fold" $ do
        "[1 2 3 4 5] 0 [+] fold" `shouldRunTo` [JInt 15]

    ---------------------------------------------------------------------
    -- Definitions
    ---------------------------------------------------------------------
    describe "definitions" $ do
      it "defines and uses square" $ do
        "[dup *] square define 5 square" `shouldRunTo` [JInt 25]

      it "defines words with DEFINE blocks" $ do
        "DEFINE square == dup * ; quad == square square . 2 quad" `shouldRunTo` [JInt 16]

      it "defines and uses cube" $ do
        "[dup dup * *] cube define 3 cube" `shouldRunTo` [JInt 27]

      it "chained definitions" $ do
        "[dup *] square define [square square] quad define 2 quad" `shouldRunTo` [JInt 16]

    ---------------------------------------------------------------------
    -- Type Predicates
    ---------------------------------------------------------------------
    describe "type predicates" $ do
      it "integer?" $ do
        "5 integer? swap pop" `shouldRunTo` [JBool True]

      it "list?" $ do
        "[1 2] list? swap pop" `shouldRunTo` [JBool True]

    ---------------------------------------------------------------------
    -- Strings
    ---------------------------------------------------------------------
    describe "strings" $ do
      it "creates strings" $ do
        "\"hello\"" `shouldRunTo` [JString "hello"]

      it "string size" $ do
        "\"hello\" size" `shouldRunTo` [JInt 5]

      it "string concat" $ do
        "\"hello\" \" world\" concat" `shouldRunTo` [JString "hello world"]

    ---------------------------------------------------------------------
    -- Edge Cases
    ---------------------------------------------------------------------
    describe "edge cases" $ do
      it "empty list" $ do
        "[]" `shouldRunTo` [JQuote []]

      it "empty list null" $ do
        "[] null" `shouldRunTo` [JBool True]

      it "zero arithmetic" $ do
        "0 5 +" `shouldRunTo` [JInt 5]
        "5 0 *" `shouldRunTo` [JInt 0]

    ---------------------------------------------------------------------
    -- Error Cases
    ---------------------------------------------------------------------
    describe "error handling" $ do
      it "reports undefined word" $ do
        shouldFail "undefined_word"

      it "reports stack underflow" $ do
        shouldFail "+"

      it "reports type error" $ do
        shouldFail "1 2 and"

      it "reports division by zero" $ do
        shouldFail "5 0 /"
