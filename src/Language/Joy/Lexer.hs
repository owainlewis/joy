--------------------------------------------------------------------
-- |
-- Module    :  Lexer
-- Copyright :  (c) Owain Lewis 2020
-- Maintainer:  Owain Lewis <owain@owainlewis.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Language.Joy.Lexer ( integer
             , float
             , whitespace
             , lexeme
             , brackets
             , braces
             ) where

import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where style = emptyDef {
    Tok.commentLine     = "#"
  , Tok.reservedOpNames = []
  , Tok.reservedNames   = ["DEFINE", "LIBRA", "HIDING", "IN"]
  }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

