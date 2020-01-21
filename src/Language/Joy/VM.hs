{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.VM
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Defines the runtime virtual machine that is used to interpret Joy programs
----------------------------------------------------------------------------
module Language.Joy.VM (run) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
import           Data.Set             (Set)

import           Language.Joy.Core    (Joy (..), Program, ProgramError (..),
                                       dup')

pluralize :: (Eq a, Num a) => a -> String -> String
pluralize n s = if n == 1 then s else s ++ "s"

instance Show ProgramError where
  show (ArityError expected actual) = mconcat
    [ "Expected stack to contain "
    , show expected
    , " "
    , pluralize expected "element"
    , " but has "
    , show actual]

type Env = Map String String

data VMS a = VMS [a] Env Bool
    deriving (Eq, Ord, Show)

instance Functor VMS where
    fmap f (VMS a b c) = VMS (fmap f a) b c

type VMState = VMS Joy
type Ex      = ExceptT ProgramError IO
type VM a    = ReaderT Program (StateT VMState Ex) a

newtype JoyMonad a = JoyMonad { unJoyMonad :: VM a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Program
           , MonadError ProgramError
           , MonadState VMState
           , MonadIO)

-- Helper to run our VM
execVM :: Program -> VMState -> JoyMonad a -> IO (Either ProgramError a)
execVM program state (JoyMonad m) =
    runExceptT . flip evalStateT state $
        runReaderT m program

modifyStack :: ([a] -> [b]) -> VMS a -> VMS b
modifyStack f (VMS a b c) = VMS (f a) b c

liftPure :: (MonadState (VMS a) m, MonadError e m) =>
     ([a] -> Either e [a]) -> m ()
liftPure f = do
    VMS a b c <- get
    case (f a) of
        Right v -> (modify . const $ VMS v b c) >> return ()
        Left e  -> throwError e

dup :: JoyMonad ()
dup = liftPure dup'

evalInstr :: Joy -> JoyMonad ()
evalInstr instr =
    case instr of
        JWord "DUP"   -> dup
        JWord "PRINT" -> get >>= (\(VMS a _ _) -> liftIO . print $ a)
        _             -> modify (push instr) >> pure ()
    where push instr =
            modifyStack (instr:)

eval :: JoyMonad ()
eval = do
  instr <- ask
  s@(VMS _ _ debug) <- get
  if debug then liftIO . print $ s else return ()
  case instr of
    []     -> return ()
    (i:is) -> evalInstr i >> local (const is) eval

-- Run a program and return either an error or the
-- run :: Program -> IO ()
run :: Program -> IO (Either ProgramError ())
run p = execVM p initState eval
    where initState = VMS [] M.empty False

runDev = run _program where
  _program = [ JInt 5
             , JWord "DUP"
             , JWord "PRINT"
             ]
