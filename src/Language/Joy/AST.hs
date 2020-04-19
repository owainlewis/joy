{-# LANGUAGE OverloadedStrings #-}
module Language.Joy.AST (Lit(..), Joy(..)) where

import           Data.Set (Set)

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
    deriving (Eq, Ord)

instance Show Joy where
  show (Literal (Boolean a))    = show a
  show (Literal (Char a))       = show a
  show (Literal (Integer a))    = show a
  show (Literal (Float a))      = show a
  show (Literal (String a))     = show a
  show (Literal (Identifier a)) = show a
  show (List a)                 = show a
  show (Definition k v)         = "define " ++ k ++ " == " ++ show v
