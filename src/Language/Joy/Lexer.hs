--------------------------------------------------------------------
-- |
-- Module    :  Lexer
-- Copyright :  (c) Owain Lewis 2020
-- License   :  MIT
-- Maintainer:  Owain Lewis <owain@owainlewis.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Lexer (lexer) where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (many)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
      ops = ["+","*","-","/",";","=",",","<",">","|",":"]
      names = []
      style = emptyDef {
        Tok.commentLine     = "#"
      , Tok.reservedOpNames = ops
      , Tok.reservedNames   = names
      }

