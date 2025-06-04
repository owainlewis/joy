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
  | Dup
  | Swap
  | Apply (v -> v -> v)
  | Apply1 (v -> v)
  | Cons
  | First
  | Rest
  | Dip
  | I  -- Execute quotation
  | Print

instance Show a => Show (Instruction a) where
  show (Push v ) = "push " ++ show v
  show (Pop    ) = "pop"
  show (Dup    ) = "dup"
  show (Swap   ) = "swap"
  show (Apply _) = "apply"
  show (Apply1 _) = "apply1"
  show (Cons   ) = "cons"
  show (First  ) = "first"
  show (Rest   ) = "rest"
  show (Dip    ) = "dip"
  show (I      ) = "i"
  show (Print  ) = "print"

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
  
  -- Pop the top value from the stack
  Pop -> do
    vm <- get
    case stack vm of
      []     -> throwError $ ArityError 1 0
      (_:xs) -> put (vm { stack = xs }) >> get >>= return
      
  -- Duplicate the top value on the stack: [X | S] => [X X | S]
  Dup -> do
    vm <- get
    case stack vm of
      []     -> throwError $ ArityError 1 0
      (x:xs) -> put (vm { stack = x : x : xs }) >> get >>= return
      
  -- Swap the top two values on the stack: [X Y | S] => [Y X | S]
  Swap -> do
    vm <- get
    case stack vm of
      (x:y:xs) -> put (vm { stack = y : x : xs }) >> get >>= return
      _        -> throwError $ ArityError 2 (length $ stack vm)
      
  -- Apply a binary function to the top two values on the stack
  Apply f -> do
    vm <- get
    case stack vm of
      (x:y:xs) -> do
        -- Note: this doesn't handle type errors yet
        put (vm { stack = f y x : xs }) 
        get >>= return
      _ -> throwError $ ArityError 2 (length $ stack vm)
      
  -- Apply a unary function to the top value on the stack
  Apply1 f -> do
    vm <- get
    case stack vm of
      (x:xs) -> do
        put (vm { stack = f x : xs })
        get >>= return
      _ -> throwError $ ArityError 1 0
      
  -- Cons: [F R | S] => [[F | R] | S]
  Cons -> do
    vm <- get
    case stack vm of
      (JQuote q:x:xs) -> put (vm { stack = JQuote (x : q) : xs }) >> get >>= return
      _ -> throwError $ ArityError 2 (length $ stack vm)
      
  -- First: [[F | R] | S] => [F | S]
  First -> do
    vm <- get
    case stack vm of
      (JQuote (x:_):xs) -> put (vm { stack = x : xs }) >> get >>= return
      _ -> throwError $ ArityError 1 (length $ stack vm)
      
  -- Rest: [[F | R] | S] => [R | S]
  Rest -> do
    vm <- get
    case stack vm of
      (JQuote (_:rs):xs) -> put (vm { stack = JQuote rs : xs }) >> get >>= return
      _ -> throwError $ ArityError 1 (length $ stack vm)
      
  -- i: [Q | S] => ... (executes quotation Q)
  I -> do
    vm <- get
    case stack vm of
      (JQuote q:xs) -> do
        put (vm { stack = xs })
        -- Run the quotation
        -- This is a simplification; a proper implementation would recurse
        return vm
      _ -> throwError $ ArityError 1 (length $ stack vm)
      
  -- dip: [Q X | S] => [X | T] (executes Q with X removed, then puts X back)
  Dip -> do
    vm <- get
    case stack vm of
      (x:JQuote q:xs) -> do
        put (vm { stack = JQuote q : xs })
        -- Execute quotation then restore x
        -- This is a simplification
        return vm
      _ -> throwError $ ArityError 2 (length $ stack vm)

eval :: JoyMonad JVM
eval = do
  instr <- ask
  case instr of
    []       -> get >>= return
    (i : is) -> evaluate i >> local (const is) eval

run :: [JoyInstruction] -> IO (Either ProgramError JVM)
run instructions = execVM instructions initState eval
  where initState = VirtualMachine { stack = [], env = M.empty }
