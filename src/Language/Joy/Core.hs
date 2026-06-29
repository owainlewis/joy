{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.Core
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Core types and error definitions for the Joy language.
-- The main Joy value type and VM operations are in VirtualMachine.
----------------------------------------------------------------------------
module Language.Joy.Core
  ( -- * Re-exports from VirtualMachine
    Joy(..)
  , Stack
  , Env
  , VMState(..)
  , VMError
  , runProgram
  , runProgramWithEnv
  , runProgramStateWithEnv
  -- * Legacy exports (for backwards compatibility)
  , Program
  , ProgramError(..)
  , vmErrorToProgramError
  )
where

import qualified Data.Text as T
import           Language.Joy.VirtualMachine
  ( Env
  , Joy(..)
  , Stack
  , VMError
  , VMState(..)
  , runProgram
  , runProgramStateWithEnv
  , runProgramWithEnv
  )
import qualified Language.Joy.VirtualMachine as VM

-- | A Joy program is a list of Joy values
type Program = [Joy]

-- | Legacy error type (mapped to VMError for compatibility)
data ProgramError
  = ArityError Int Int        -- ^ (expected, actual)
  | TypeError String String   -- ^ (expected, actual)
  | UndefinedError String     -- ^ undefined operation
  deriving (Eq, Ord)

instance Show ProgramError where
  show (ArityError expected actual) =
    "ArityError: Expected " ++ show expected ++ " elements, but got " ++ show actual
  show (TypeError expected actual) =
    "TypeError: Expected " ++ expected ++ ", but got " ++ actual
  show (UndefinedError name) =
    "UndefinedError: " ++ name ++ " is not defined"

-- | Convert VMError to ProgramError
vmErrorToProgramError :: VMError -> ProgramError
vmErrorToProgramError (VM.StackUnderflow _ expected actual) = ArityError expected actual
vmErrorToProgramError (VM.TypeError _ expected actual) =
  TypeError (T.unpack expected) (T.unpack actual)
vmErrorToProgramError (VM.UndefinedWord w) = UndefinedError (T.unpack w)
vmErrorToProgramError VM.DivisionByZero = TypeError "non-zero" "zero"
vmErrorToProgramError (VM.EmptyQuotation _) = ArityError 1 0
vmErrorToProgramError (VM.RuntimeError msg) = UndefinedError (T.unpack msg)
