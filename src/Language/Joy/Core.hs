{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.Core
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Basic AST and general structural operations for stacks
----------------------------------------------------------------------------

module Language.Joy.Core
  ( Joy(..)
  , Program
  , ProgramError(..)
  , dup'
  )
where

import qualified Data.Text                     as T

data Instr =
    DUP
  | SWAP
  | PRINT
    deriving (Eq, Ord, Show)

data Joy =
    JWord T.Text
  | JString T.Text
  | JInt Integer
  | JFloat Double
  | JBool Bool
  | JChar Char
  | JQuote [Joy]
  | JInstruction Instr
    deriving (Eq, Ord, Show)

type Program = [Joy]

type SimpleFunction = [Joy] -> Either ProgramError [Joy]

data ProgramError =
    ArityError Int Int -- (expected, actual)
    deriving (Eq, Ord)

withN :: Foldable t => Int -> (t a -> b) -> t a -> Either ProgramError b
withN n f s =
  let l = length s in if l < n then Left $ ArityError n l else Right (f s)

-- DEFINE dup as [X | S]  =>  [X X | S]
dup' :: [Joy] -> Either ProgramError [Joy]
dup' s = withN 1 f s where f (x : xs) = x : x : xs
