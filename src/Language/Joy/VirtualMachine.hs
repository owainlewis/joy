{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.VirtualMachine
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Defines the virtual machine that is used to interpret Joy programs
--
-- This machine has a ‘stack’ and can execute ‘instructions’ which change the value of the stack.
-- An instruction either pushes a value of type v on the stack, or it executes an operator
-- that takes the two top values of the stack, applies the operator, and pushes the result
-- back on the stack.
--
----------------------------------------------------------------------------
module Language.Joy.VirtualMachine where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Language.Joy.Core              ( Joy(..)
                                                , Program
                                                , ProgramError(..)
                                                )

-- | The virtual machine instruction set
data Instruction v =
    Push v
  | Pop
  | Apply (v -> v -> v)
  | Print

instance Show a => Show (Instruction a) where
  show (Push v ) = "push " ++ show v
  show (Pop    ) = "pop "
  show (Apply _) = "apply"

-- | Type alias for the virtual machine environment
type Env = Map T.Text T.Text

-- | A VirtualMachine which performs instructions that modify a stack. A global environment is available
--  to store temporary program state
data VirtualMachine a = VirtualMachine {
    stack :: [a]
  , env   :: Env
} deriving ( Show )

-- Update the virtual machine stack by applying a function over it (fmap)
modifyStack
  :: VirtualMachine a -> ((VirtualMachine b -> [b]) -> [c]) -> VirtualMachine c
modifyStack vm f = vm { stack = f stack }

type JoyInstruction = Instruction Joy

type JVM = VirtualMachine Joy
type Ex = ExceptT ProgramError IO
type VM a = ReaderT [JoyInstruction] (StateT JVM Ex) a

newtype JoyMonad a = JoyMonad { unJoyMonad :: VM a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader [JoyInstruction]
           , MonadError ProgramError
           , MonadState JVM
           , MonadIO)

execVM :: [JoyInstruction] -> JVM -> JoyMonad a -> IO (Either ProgramError a)
execVM program state (JoyMonad m) =
  runExceptT . flip evalStateT state $ runReaderT m program

evaluate :: JoyInstruction -> JoyMonad JVM
evaluate instr = case instr of
    -- Print the current virtual machine stack
  Print -> do
    vm <- get
    liftIO . print . show $ (stack vm)
    return vm
  -- Push a value onto the virtual machine stack
  Push x -> modify (\vm -> vm { stack = (x : stack vm) }) >> get >>= return

eval :: JoyMonad JVM
eval = do
  instr <- ask
  case instr of
    []       -> get >>= return
    (i : is) -> evaluate i >> local (const is) eval

run :: [JoyInstruction] -> IO (Either ProgramError JVM)
run instructions = execVM instructions initState eval
  where initState = VirtualMachine { stack = [], env = M.empty }
