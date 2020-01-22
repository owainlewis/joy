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

import           Data.Map          (Map)
import qualified Data.Text         as T
import           Language.Joy.Core (Joy (..), Program, ProgramError (..))

data Instruction v =
    Push v
  | Pop
  | Apply (v -> v -> v)

type Env = Map T.Text T.Text

data VirtualMachine a = VirtualMachine {
    stack :: [a]
  , env   :: Env
}
