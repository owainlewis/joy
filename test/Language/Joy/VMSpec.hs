{-# LANGUAGE OverloadedStrings #-}
module Language.Joy.VMSpec (spec) where

import           Language.Joy.VirtualMachine
import           Test.Hspec
import qualified Data.Text as T

-- Helper to run a program and check the result
shouldEvalTo :: [Joy] -> [Joy] -> Expectation
shouldEvalTo program expected =
  case runProgram program of
    Left err -> expectationFailure $ "Unexpected error: " ++ show err
    Right stack -> stack `shouldBe` expected

-- Helper to check for expected error
shouldFailWith :: [Joy] -> (VMError -> Bool) -> Expectation
shouldFailWith program predicate =
  case runProgram program of
    Left err -> if predicate err
                  then return ()
                  else expectationFailure $ "Got unexpected error: " ++ show err
    Right stack -> expectationFailure $ "Expected error but got: " ++ show stack

spec :: Spec
spec = do
  describe "Virtual Machine" $ do
    ---------------------------------------------------------------------
    -- Stack Operations
    ---------------------------------------------------------------------
    describe "stack operations" $ do
      it "pushes literals onto the stack" $ do
        [JInt 10, JInt 20] `shouldEvalTo` [JInt 20, JInt 10]

      it "dup duplicates the top value" $ do
        [JInt 10, JWord "dup"] `shouldEvalTo` [JInt 10, JInt 10]

      it "pop removes the top value" $ do
        [JInt 10, JInt 20, JWord "pop"] `shouldEvalTo` [JInt 10]

      it "swap exchanges the top two values" $ do
        [JInt 10, JInt 20, JWord "swap"] `shouldEvalTo` [JInt 10, JInt 20]

      it "rollup rotates three values: X Y Z -> Z X Y" $ do
        [JInt 1, JInt 2, JInt 3, JWord "rollup"] `shouldEvalTo` [JInt 2, JInt 1, JInt 3]

      it "rolldown rotates three values: X Y Z -> Y Z X" $ do
        [JInt 1, JInt 2, JInt 3, JWord "rolldown"] `shouldEvalTo` [JInt 1, JInt 3, JInt 2]

      it "rotate reverses three values: X Y Z -> Z Y X" $ do
        [JInt 1, JInt 2, JInt 3, JWord "rotate"] `shouldEvalTo` [JInt 1, JInt 2, JInt 3]

      it "dupd duplicates second element" $ do
        [JInt 1, JInt 2, JWord "dupd"] `shouldEvalTo` [JInt 2, JInt 1, JInt 1]

    ---------------------------------------------------------------------
    -- Arithmetic Operations
    ---------------------------------------------------------------------
    describe "arithmetic operations" $ do
      it "adds two integers" $ do
        [JInt 2, JInt 3, JWord "+"] `shouldEvalTo` [JInt 5]

      it "subtracts two integers" $ do
        [JInt 10, JInt 3, JWord "-"] `shouldEvalTo` [JInt 7]

      it "multiplies two integers" $ do
        [JInt 4, JInt 5, JWord "*"] `shouldEvalTo` [JInt 20]

      it "divides two integers (float result)" $ do
        [JInt 10, JInt 4, JWord "/"] `shouldEvalTo` [JFloat 2.5]

      it "integer division" $ do
        [JInt 10, JInt 3, JWord "div"] `shouldEvalTo` [JInt 3]

      it "modulo" $ do
        [JInt 10, JInt 3, JWord "%"] `shouldEvalTo` [JInt 1]

      it "handles mixed int/float arithmetic" $ do
        [JInt 2, JFloat 3.5, JWord "+"] `shouldEvalTo` [JFloat 5.5]

      it "negates a number" $ do
        [JInt 5, JWord "neg"] `shouldEvalTo` [JInt (-5)]

      it "computes absolute value" $ do
        [JInt (-5), JWord "abs"] `shouldEvalTo` [JInt 5]

      it "succ increments" $ do
        [JInt 5, JWord "succ"] `shouldEvalTo` [JInt 6]

      it "pred decrements" $ do
        [JInt 5, JWord "pred"] `shouldEvalTo` [JInt 4]

      it "max returns larger value" $ do
        [JInt 3, JInt 7, JWord "max"] `shouldEvalTo` [JInt 7]

      it "min returns smaller value" $ do
        [JInt 3, JInt 7, JWord "min"] `shouldEvalTo` [JInt 3]

      it "catches division by zero" $ do
        [JInt 10, JInt 0, JWord "/"] `shouldFailWith` (== DivisionByZero)

    ---------------------------------------------------------------------
    -- Comparison Operations
    ---------------------------------------------------------------------
    describe "comparison operations" $ do
      it "less than" $ do
        [JInt 3, JInt 5, JWord "<"] `shouldEvalTo` [JBool True]
        [JInt 5, JInt 3, JWord "<"] `shouldEvalTo` [JBool False]

      it "greater than" $ do
        [JInt 5, JInt 3, JWord ">"] `shouldEvalTo` [JBool True]
        [JInt 3, JInt 5, JWord ">"] `shouldEvalTo` [JBool False]

      it "less than or equal" $ do
        [JInt 3, JInt 3, JWord "<="] `shouldEvalTo` [JBool True]
        [JInt 3, JInt 5, JWord "<="] `shouldEvalTo` [JBool True]

      it "greater than or equal" $ do
        [JInt 5, JInt 5, JWord ">="] `shouldEvalTo` [JBool True]
        [JInt 5, JInt 3, JWord ">="] `shouldEvalTo` [JBool True]

      it "equality" $ do
        [JInt 5, JInt 5, JWord "="] `shouldEvalTo` [JBool True]
        [JInt 5, JInt 3, JWord "="] `shouldEvalTo` [JBool False]

      it "inequality" $ do
        [JInt 5, JInt 3, JWord "!="] `shouldEvalTo` [JBool True]
        [JInt 5, JInt 5, JWord "!="] `shouldEvalTo` [JBool False]

    ---------------------------------------------------------------------
    -- Boolean Operations
    ---------------------------------------------------------------------
    describe "boolean operations" $ do
      it "and" $ do
        [JBool True, JBool True, JWord "and"] `shouldEvalTo` [JBool True]
        [JBool True, JBool False, JWord "and"] `shouldEvalTo` [JBool False]

      it "or" $ do
        [JBool False, JBool True, JWord "or"] `shouldEvalTo` [JBool True]
        [JBool False, JBool False, JWord "or"] `shouldEvalTo` [JBool False]

      it "not" $ do
        [JBool True, JWord "not"] `shouldEvalTo` [JBool False]
        [JBool False, JWord "not"] `shouldEvalTo` [JBool True]

      it "xor" $ do
        [JBool True, JBool False, JWord "xor"] `shouldEvalTo` [JBool True]
        [JBool True, JBool True, JWord "xor"] `shouldEvalTo` [JBool False]

    ---------------------------------------------------------------------
    -- List Operations
    ---------------------------------------------------------------------
    describe "list operations" $ do
      it "cons prepends to a list" $ do
        [JInt 1, JQuote [JInt 2, JInt 3], JWord "cons"]
          `shouldEvalTo` [JQuote [JInt 1, JInt 2, JInt 3]]

      it "swons swaps and conses" $ do
        [JQuote [JInt 2, JInt 3], JInt 1, JWord "swons"]
          `shouldEvalTo` [JQuote [JInt 1, JInt 2, JInt 3]]

      it "first extracts head" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JWord "first"]
          `shouldEvalTo` [JInt 1]

      it "rest extracts tail" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JWord "rest"]
          `shouldEvalTo` [JQuote [JInt 2, JInt 3]]

      it "uncons splits head and tail" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JWord "uncons"]
          `shouldEvalTo` [JQuote [JInt 2, JInt 3], JInt 1]

      it "concat joins two lists" $ do
        [JQuote [JInt 1, JInt 2], JQuote [JInt 3, JInt 4], JWord "concat"]
          `shouldEvalTo` [JQuote [JInt 1, JInt 2, JInt 3, JInt 4]]

      it "size returns length" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JWord "size"]
          `shouldEvalTo` [JInt 3]

      it "null tests for empty" $ do
        [JQuote [], JWord "null"] `shouldEvalTo` [JBool True]
        [JQuote [JInt 1], JWord "null"] `shouldEvalTo` [JBool False]

      it "reverse reverses a list" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JWord "reverse"]
          `shouldEvalTo` [JQuote [JInt 3, JInt 2, JInt 1]]

      it "at gets element at index" $ do
        [JQuote [JInt 10, JInt 20, JInt 30], JInt 1, JWord "at"]
          `shouldEvalTo` [JInt 20]

      it "take gets first N elements" $ do
        [JQuote [JInt 1, JInt 2, JInt 3, JInt 4], JInt 2, JWord "take"]
          `shouldEvalTo` [JQuote [JInt 1, JInt 2]]

      it "drop removes first N elements" $ do
        [JQuote [JInt 1, JInt 2, JInt 3, JInt 4], JInt 2, JWord "drop"]
          `shouldEvalTo` [JQuote [JInt 3, JInt 4]]

    ---------------------------------------------------------------------
    -- Quotation Execution
    ---------------------------------------------------------------------
    describe "quotation execution" $ do
      it "i executes a quotation" $ do
        [JInt 5, JQuote [JWord "dup", JWord "*"], JWord "i"]
          `shouldEvalTo` [JInt 25]

      it "dip executes under top" $ do
        [JInt 1, JInt 2, JQuote [JInt 10, JWord "+"], JWord "dip"]
          `shouldEvalTo` [JInt 2, JInt 11]

      it "x duplicates and executes" $ do
        [JInt 3, JQuote [JWord "dup", JWord "*"], JWord "x"]
          `shouldEvalTo` [JInt 9, JQuote [JWord "dup", JWord "*"]]

      it "unit wraps in a list" $ do
        [JInt 5, JWord "unit"] `shouldEvalTo` [JQuote [JInt 5]]

      it "pair creates a two-element list" $ do
        [JInt 1, JInt 2, JWord "pair"] `shouldEvalTo` [JQuote [JInt 1, JInt 2]]

    ---------------------------------------------------------------------
    -- Conditionals
    ---------------------------------------------------------------------
    describe "conditionals" $ do
      it "choice selects based on boolean" $ do
        [JBool True, JInt 1, JInt 2, JWord "choice"]
          `shouldEvalTo` [JInt 1]
        [JBool False, JInt 1, JInt 2, JWord "choice"]
          `shouldEvalTo` [JInt 2]

      it "branch executes based on boolean" $ do
        [JBool True, JQuote [JInt 1], JQuote [JInt 2], JWord "branch"]
          `shouldEvalTo` [JInt 1]
        [JBool False, JQuote [JInt 1], JQuote [JInt 2], JWord "branch"]
          `shouldEvalTo` [JInt 2]

      it "ifte conditional execution" $ do
        -- 5 > 3, so execute then branch
        [JInt 5, JQuote [JInt 3, JWord ">"], JQuote [JInt 100], JQuote [JInt 0], JWord "ifte"]
          `shouldEvalTo` [JInt 100]
        -- 2 > 3 is false, so execute else branch
        [JInt 2, JQuote [JInt 3, JWord ">"], JQuote [JInt 100], JQuote [JInt 0], JWord "ifte"]
          `shouldEvalTo` [JInt 0]

    ---------------------------------------------------------------------
    -- Higher-Order Combinators
    ---------------------------------------------------------------------
    describe "higher-order combinators" $ do
      it "map applies quotation to each element" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JQuote [JWord "dup", JWord "*"], JWord "map"]
          `shouldEvalTo` [JQuote [JInt 1, JInt 4, JInt 9]]

      it "filter selects elements" $ do
        [JQuote [JInt 1, JInt 2, JInt 3, JInt 4], JQuote [JInt 2, JWord ">"], JWord "filter"]
          `shouldEvalTo` [JQuote [JInt 3, JInt 4]]

      it "fold reduces a list" $ do
        [JQuote [JInt 1, JInt 2, JInt 3, JInt 4], JInt 0, JQuote [JWord "+"], JWord "fold"]
          `shouldEvalTo` [JInt 10]

      it "step applies to each element" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JQuote [JWord "dup", JWord "*"], JWord "step"]
          `shouldEvalTo` [JInt 9, JInt 4, JInt 1]

      it "times repeats N times" $ do
        [JInt 1, JInt 3, JQuote [JInt 2, JWord "*"], JWord "times"]
          `shouldEvalTo` [JInt 8]

    ---------------------------------------------------------------------
    -- Recursion Combinators
    ---------------------------------------------------------------------
    describe "recursion combinators" $ do
      it "linrec implements factorial" $ do
        -- factorial 5 = 120
        [ JInt 5
        , JQuote [JInt 0, JWord "="]      -- if n == 0
        , JQuote [JWord "pop", JInt 1]    -- then 1
        , JQuote [JWord "dup", JInt 1, JWord "-"]  -- else n, n-1
        , JQuote [JWord "*"]              -- combine with *
        , JWord "linrec"
        ] `shouldEvalTo` [JInt 120]

      it "tailrec implements sum" $ do
        -- sum from 5 down to 0: 5+4+3+2+1+0 = 15
        [ JInt 5, JInt 0  -- n, accumulator
        , JQuote [JWord "swap", JInt 0, JWord "="]  -- if n == 0
        , JQuote [JWord "pop"]                       -- then return acc
        , JQuote [JWord "swap", JWord "dup", JInt 1, JWord "-", JWord "swap", JWord "rolldown", JWord "+"]  -- else decrement and add
        , JWord "tailrec"
        ] `shouldEvalTo` [JInt 15]

    ---------------------------------------------------------------------
    -- Type Predicates
    ---------------------------------------------------------------------
    describe "type predicates" $ do
      it "integer? tests for integer" $ do
        [JInt 5, JWord "integer?"] `shouldEvalTo` [JBool True, JInt 5]
        [JFloat 5.0, JWord "integer?"] `shouldEvalTo` [JBool False, JFloat 5.0]

      it "float? tests for float" $ do
        [JFloat 5.0, JWord "float?"] `shouldEvalTo` [JBool True, JFloat 5.0]
        [JInt 5, JWord "float?"] `shouldEvalTo` [JBool False, JInt 5]

      it "list? tests for quotation" $ do
        [JQuote [JInt 1], JWord "list?"] `shouldEvalTo` [JBool True, JQuote [JInt 1]]
        [JInt 5, JWord "list?"] `shouldEvalTo` [JBool False, JInt 5]

      it "string? tests for string" $ do
        [JString "hello", JWord "string?"] `shouldEvalTo` [JBool True, JString "hello"]
        [JInt 5, JWord "string?"] `shouldEvalTo` [JBool False, JInt 5]

    ---------------------------------------------------------------------
    -- Definitions
    ---------------------------------------------------------------------
    describe "definitions" $ do
      it "define creates a new word" $ do
        [ JQuote [JWord "dup", JWord "*"]
        , JWord "square"
        , JWord "define"
        , JInt 5
        , JWord "square"
        ] `shouldEvalTo` [JInt 25]

      it "definitions can use other definitions" $ do
        [ JQuote [JWord "dup", JWord "*"], JWord "square", JWord "define"
        , JQuote [JWord "square", JWord "square"], JWord "quad", JWord "define"
        , JInt 2
        , JWord "quad"
        ] `shouldEvalTo` [JInt 16]

    ---------------------------------------------------------------------
    -- Error Handling
    ---------------------------------------------------------------------
    describe "error handling" $ do
      it "reports stack underflow in pop" $ do
        [JWord "pop"] `shouldFailWith` \case
          StackUnderflow _ _ _ -> True
          _ -> False

      it "reports stack underflow in dup" $ do
        [JWord "dup"] `shouldFailWith` \case
          StackUnderflow _ _ _ -> True
          _ -> False

      it "reports undefined word" $ do
        [JWord "undefined_word"] `shouldFailWith` \case
          UndefinedWord _ -> True
          _ -> False

      it "reports type error" $ do
        [JInt 1, JInt 2, JWord "and"] `shouldFailWith` \case
          TypeError _ _ _ -> True
          _ -> False

    ---------------------------------------------------------------------
    -- Miscellaneous
    ---------------------------------------------------------------------
    describe "miscellaneous" $ do
      it "infra executes on a temporary stack" $ do
        [JQuote [JInt 1, JInt 2, JInt 3], JQuote [JWord "+"], JWord "infra"]
          `shouldEvalTo` [JQuote [JInt 5, JInt 1]]

      it "cleave applies two quotations to same value" $ do
        [JInt 5, JQuote [JInt 2, JWord "+"], JQuote [JInt 2, JWord "*"], JWord "cleave"]
          `shouldEvalTo` [JInt 10, JInt 7]
