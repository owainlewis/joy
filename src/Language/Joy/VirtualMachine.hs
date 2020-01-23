{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.VirtualMachine
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Defines the virtual machine that is used to interpret Joy programs
--
-- This machine has a ‘stack’ and can execute ‘instructions’ which change the value of the stack.
-- An instruction either pushes a value of type v on the stack, or it executes an operator
-- that takes the two top values of the stack, applies the operator, and pushes the result
-- back on the stack. A
--
----------------------------------------------------------------------------
module Language.Joy.VirtualMachine where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map             (Map)
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Language.Joy.Core    (Joy (..), Program, ProgramError (..))

data Instruction v =
    Push v
  | Pop
  | Apply (v -> v -> v)

type JoyInstruction = Instruction Joy

type Env = Map T.Text T.Text

data VirtualMachine a = VirtualMachine {
    stack :: [a]
  , env   :: Env
} deriving ( Show )

type JVM = VirtualMachine Joy
type Ex      = ExceptT ProgramError IO
type VM a    = ReaderT [JoyInstruction] (StateT JVM Ex) a

newtype JoyMonad a = JoyMonad { unJoyMonad :: VM a }
  deriving ( Functor
           , Applicative
           , Monad
           -- Reads the incoming instruction set
           , MonadReader [JoyInstruction]
           , MonadError ProgramError
           , MonadState JVM
           , MonadIO)

execVM :: [JoyInstruction] -> JVM -> JoyMonad a -> IO (Either ProgramError a)
execVM program state (JoyMonad m) = runExceptT . flip evalStateT state $ runReaderT m program

eval :: JoyMonad JVM
eval = do
  instr <- ask
  case instr of
    []     -> get >>= return
    (i:is) -> local (const is) eval

run :: [JoyInstruction] -> IO (Either ProgramError JVM)
run instructions = execVM instructions initState eval
    where initState = VirtualMachine { stack = [], env = M.empty }
