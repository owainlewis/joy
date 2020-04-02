{-# LANGUAGE OverloadedStrings #-}
module Language.Joy.AST (Term(..))where

type Name = String

data Term = Word Name
          | List [Term]
          | Number Integer
          | Str String
          | Func Name [Term]
    deriving (Eq, Ord, Show)
