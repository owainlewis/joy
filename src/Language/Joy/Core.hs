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
  , withN
  , dup'
  , swap'
  , pop'
  , cons'
  , first'
  , rest'
  , i'
  )
where

import qualified Data.Text                     as T
import Data.List (intercalate)

data Instr =
    DUP
  | SWAP
  | POP
  | CONS
  | FIRST
  | REST
  | I
  | DIP
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
  | TypeError String String -- (expected, actual)
  | UndefinedError String -- undefined operation
    deriving (Eq, Ord)

instance Show ProgramError where
  show (ArityError expected actual) = 
    "ArityError: Expected " ++ show expected ++ " elements, but got " ++ show actual
  show (TypeError expected actual) = 
    "TypeError: Expected " ++ expected ++ ", but got " ++ actual
  show (UndefinedError name) = 
    "UndefinedError: " ++ name ++ " is not defined"

-- Helper to extract a Joy value's type as a string
typeOf :: Joy -> String
typeOf (JWord _) = "word"
typeOf (JString _) = "string"
typeOf (JInt _) = "integer"
typeOf (JFloat _) = "float"
typeOf (JBool _) = "boolean"
typeOf (JChar _) = "char"
typeOf (JQuote _) = "quotation"
typeOf (JInstruction _) = "instruction"

-- Helper for operations that need N stack elements
withN :: Foldable t => Int -> (t a -> b) -> t a -> Either ProgramError b
withN n f s =
  let l = length s in if l < n then Left $ ArityError n l else Right (f s)

-- Core stack operations

-- DEFINE dup as [X | S]  =>  [X X | S]
dup' :: [Joy] -> Either ProgramError [Joy]
dup' s = withN 1 f s where f (x : xs) = x : x : xs

-- DEFINE swap as [X Y | S]  =>  [Y X | S]
swap' :: [Joy] -> Either ProgramError [Joy]
swap' s = withN 2 f s where f (x : y : xs) = y : x : xs

-- DEFINE pop as [X | S]  =>  S
pop' :: [Joy] -> Either ProgramError [Joy]
pop' s = withN 1 f s where f (_ : xs) = xs

-- DEFINE cons as [X [Y...] | S]  =>  [[X Y...] | S]
cons' :: [Joy] -> Either ProgramError [Joy]
cons' s = withN 2 f s 
  where 
    f (q@(JQuote ys) : x : xs) = JQuote (x : ys) : xs
    f (y : x : _) = Left $ TypeError "quotation" (typeOf y)

-- DEFINE first as [[X Y...] | S]  =>  [X | S]
first' :: [Joy] -> Either ProgramError [Joy]
first' s = withN 1 f s 
  where 
    f (JQuote (x:_) : xs) = x : xs
    f (JQuote [] : _) = Left $ ArityError 1 0
    f (y : _) = Left $ TypeError "quotation" (typeOf y)

-- DEFINE rest as [[X Y...] | S]  =>  [[Y...] | S]
rest' :: [Joy] -> Either ProgramError [Joy]
rest' s = withN 1 f s 
  where 
    f (JQuote (_:ys) : xs) = JQuote ys : xs
    f (JQuote [] : _) = Left $ ArityError 1 0
    f (y : _) = Left $ TypeError "quotation" (typeOf y)

-- DEFINE i as [[P] | S]  =>  execute P on S
-- This is a simplification - in reality we would recurse into P
i' :: [Joy] -> Either ProgramError [Joy]
i' s = withN 1 f s 
  where 
    f (JQuote _ : xs) = xs  -- Simplified - should execute the quotation
    f (y : _) = Left $ TypeError "quotation" (typeOf y)
