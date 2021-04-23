{-# LANGUAGE OverloadedStrings #-}
module Language.Joy.AST
  ( Lit(..)
  , Joy(..)
  )
where

import           Data.Set                       ( Set )

-- See http://www.kevinalbrecht.com/code/joy-mirror/plain-manual.html
data Lit = Boolean Bool
         | Char Char
         | Integer Integer
         | Float Double
         | String String
         | Identifier String
    deriving (Eq, Ord, Show)

data Joy = Literal Lit
         | List [Joy]
         | Definition String [Joy]
         | DefinitionList [Joy]
    deriving (Eq, Ord, Show)
