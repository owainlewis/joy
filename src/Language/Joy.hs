{-# LANGUAGE OverloadedStrings #-}
module Language.Joy 
  ( runJoy
  , evalJoy
  , runJoyFile
  )
where

import qualified Control.Arrow                 as Arrow
import           Data.Either                    (either)
import qualified Data.Text                     as T
import           Language.Joy.AST
import           Language.Joy.Core              (ProgramError(..), Joy(..), Program)
import           Language.Joy.Parser
import           Language.Joy.VirtualMachine    (JoyInstruction(..), run, Push)

-- | Parse Joy source code
runJoy :: String -> Either String [Joy]
runJoy s = Arrow.left show $ readJoyExpr s

-- | Parse and evaluate Joy source code
evalJoy :: String -> IO (Either ProgramError [JoyInstruction])
evalJoy s = do
  case readJoyExpr s of
    Left err -> return $ Left (ArityError 0 0) -- Replace with better error handling
    Right exprs -> do
      let instructions = concatMap astToInstructions exprs
      result <- run instructions
      return $ Right instructions

-- | Run a Joy program from a file
runJoyFile :: FilePath -> IO (Either String [Joy])
runJoyFile path = do
  result <- readJoyFile path
  return $ Arrow.left show result

-- | Convert AST to VM instructions
astToInstructions :: Joy -> [JoyInstruction]
astToInstructions (Literal (Boolean b)) = [Push (JBool b)]
astToInstructions (Literal (Char c)) = [Push (JChar c)]
astToInstructions (Literal (Integer i)) = [Push (JInt i)]
astToInstructions (Literal (Float f)) = [Push (JFloat f)]
astToInstructions (Literal (String s)) = [Push (JString (T.pack s))]
astToInstructions (Literal (Identifier i)) = [Push (JWord (T.pack i))]
astToInstructions (List js) = [Push (JQuote (map astToCore js))]
astToInstructions (Definition name forms) = 
  -- For now, just convert the body to instructions
  concatMap astToInstructions forms
astToInstructions (DefinitionList defs) = 
  concatMap astToInstructions defs

-- | Convert AST Joy to Core Joy
astToCore :: Joy -> Core.Joy
astToCore (Literal (Boolean b)) = JBool b
astToCore (Literal (Char c)) = JChar c
astToCore (Literal (Integer i)) = JInt i
astToCore (Literal (Float f)) = JFloat f
astToCore (Literal (String s)) = JString (T.pack s)
astToCore (Literal (Identifier i)) = JWord (T.pack i)
astToCore (List js) = JQuote (map astToCore js)
astToCore (Definition _ _) = JWord (T.pack "definition")
astToCore (DefinitionList _) = JWord (T.pack "definition-list")
