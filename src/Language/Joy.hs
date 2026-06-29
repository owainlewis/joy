{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Joy is a concatenative, stack-based programming language.
-- This module provides the main API for parsing and executing Joy programs.
--
-- Example usage:
--
-- >>> runJoy "1 2 +"
-- Right [3]
--
-- >>> runJoy "[1 2 3] [dup *] map"
-- Right [[1, 4, 9]]
--
----------------------------------------------------------------------------
module Language.Joy
  ( -- * Running Joy programs
    runJoy
  , runJoyFile
  , evalJoy
    -- * Types
  , Joy(..)
  , Stack
  , Env
  , VMError(..)
  , VMState(..)
    -- * Lower-level API
  , parseJoy
  , astToJoy
  , astToProgram
  , runProgram
  , runProgramWithEnv
  , runProgramStateWithEnv
  )
where

import qualified Control.Arrow                 as Arrow
import qualified Data.Text                     as T
import           Language.Joy.AST              (Lit(..))
import qualified Language.Joy.AST              as AST
import           Language.Joy.Parser           (readJoyExpr)
import           Language.Joy.VirtualMachine   ( Joy(..)
                                               , Env
                                               , Stack
                                               , VMError(..)
                                               , VMState(..)
                                               , runProgram
                                               , runProgramStateWithEnv
                                               , runProgramWithEnv
                                               )
import qualified Language.Joy.VirtualMachine   as VM

-- | Parse Joy source code into AST
parseJoy :: String -> Either String [AST.Joy]
parseJoy s = Arrow.left show $ readJoyExpr s

-- | Convert AST Joy to VM Joy value
astToJoy :: AST.Joy -> VM.Joy
astToJoy (AST.Literal (Boolean b))    = VM.JBool b
astToJoy (AST.Literal (Char c))       = VM.JChar c
astToJoy (AST.Literal (Integer i))    = VM.JInt i
astToJoy (AST.Literal (Float f))      = VM.JFloat f
astToJoy (AST.Literal (String s))     = VM.JString (T.pack s)
astToJoy (AST.Literal (Identifier i)) = VM.JWord (T.pack i)
astToJoy (AST.List js)                = VM.JQuote (astToProgram js)
astToJoy (AST.Definition name body) =
  VM.JQuote [VM.JQuote (astToProgram body), VM.JWord (T.pack name), VM.JWord "define"]
astToJoy (AST.DefinitionList defs) = VM.JQuote (concatMap definitionToProgram defs)

-- | Convert AST to a flat list of VM Joy values (a program)
astToProgram :: [AST.Joy] -> [VM.Joy]
astToProgram = concatMap astNodeToProgram

astNodeToProgram :: AST.Joy -> [VM.Joy]
astNodeToProgram (AST.Definition name body) = definitionToProgram (AST.Definition name body)
astNodeToProgram (AST.DefinitionList defs)  = concatMap definitionToProgram defs
astNodeToProgram node                       = [astToJoy node]

definitionToProgram :: AST.Joy -> [VM.Joy]
definitionToProgram (AST.Definition name body) =
  [VM.JQuote (astToProgram body), VM.JWord (T.pack name), VM.JWord "define"]
definitionToProgram node = [astToJoy node]

-- | Parse and run a Joy program, returning the final stack
--
-- >>> runJoy "1 2 +"
-- Right [3]
--
-- >>> runJoy "5 [0 =] [1] [dup 1 - swap [*] dip] ifte"
-- Right [120]
runJoy :: String -> Either String Stack
runJoy s = do
  ast <- parseJoy s
  let program = astToProgram ast
  Arrow.left show $ runProgram program

-- | Parse and run a Joy program from a file
runJoyFile :: FilePath -> IO (Either String Stack)
runJoyFile path = do
  contents <- readFile path
  return $ runJoy contents

-- | Evaluate Joy source and return the final stack (IO version for backwards compat)
evalJoy :: String -> IO (Either VMError Stack)
evalJoy s = do
  case parseJoy s of
    Left err -> return $ Left (RuntimeError (T.pack err))
    Right ast -> do
      let program = astToProgram ast
      return $ runProgram program
