{-# LANGUAGE OverloadedStrings #-}
module Language.Joy.AST (Term(..))where

-- See http://www.kevinalbrecht.com/code/joy-mirror/plain-manual.html
data Lit = Boolean Bool
         | Char Char
         | Integer Integer
         | Float Float
         | String String
         | Identifier String
         | List [Lit]
    deriving (Eq, Ord, Show)
