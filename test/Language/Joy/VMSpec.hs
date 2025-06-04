module Language.Joy.VMSpec (spec) where

import           Language.Joy.Core
import           Language.Joy.VirtualMachine

import           Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Virtual Machine" $ do
    describe "basic operations" $ do
      it "pushes values onto the stack" $ do
        result <- run [Push (JInt 10), Push (JInt 20)]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JInt 20, JInt 10]
      
      it "pops values from the stack" $ do
        result <- run [Push (JInt 10), Push (JInt 20), Pop]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JInt 10]
      
      it "duplicates the top value" $ do
        result <- run [Push (JInt 10), Dup]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JInt 10, JInt 10]
      
      it "swaps the top two values" $ do
        result <- run [Push (JInt 10), Push (JInt 20), Swap]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JInt 10, JInt 20]

    describe "list operations" $ do
      it "builds a quotation with cons" $ do
        result <- run [Push (JQuote []), Push (JInt 10), Cons]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JQuote [JInt 10]]
      
      it "extracts the first element from a quotation" $ do
        result <- run [Push (JQuote [JInt 10, JInt 20]), First]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JInt 10]
      
      it "extracts the rest of a quotation" $ do
        result <- run [Push (JQuote [JInt 10, JInt 20, JInt 30]), Rest]
        case result of
          Left err -> fail $ "Should not fail: " ++ show err
          Right vm -> stack vm `shouldBe` [JQuote [JInt 20, JInt 30]]

    describe "error handling" $ do
      it "handles stack underflow in pop" $ do
        result <- run [Pop]
        case result of
          Left (ArityError 1 0) -> return ()
          _ -> fail "Should fail with ArityError"
      
      it "handles stack underflow in dup" $ do
        result <- run [Dup]
        case result of
          Left (ArityError 1 0) -> return ()
          _ -> fail "Should fail with ArityError"
      
      it "handles stack underflow in swap" $ do
        result <- run [Push (JInt 10), Swap]
        case result of
          Left (ArityError 2 1) -> return ()
          _ -> fail "Should fail with ArityError"