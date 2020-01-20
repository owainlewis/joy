{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.Parser
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Parser for the Joy programming language
----------------------------------------------------------------------------
module Language.Joy.Parser (readExpr) where

import           Text.Parsec
import qualified Text.Parsec.Language  as Lang
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tok

import           Control.Monad         (mzero)
import           Data.Functor.Identity (Identity)
import qualified Data.Text             as T
import           Language.Joy.Core     (Joy (..))

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
    Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = ";"
  , Tok.opStart = mzero
  , Tok.opLetter = mzero
  -- Identifiers must start with
  , Tok.identStart = letter <|> oneOf "@"
  -- And be formed by ...
  , Tok.identLetter = digit <|> letter <|> oneOf "!@"
  }

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

brackets :: ParsecT T.Text () Identity a -> ParsecT T.Text () Identity a
brackets = Tok.brackets lexer

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer) <?> "identifier"

-- | ----------------------------------------
-- | Language Parsers
-- | ----------------------------------------
parseInteger :: Parser Joy
parseInteger = JInt <$> Tok.integer lexer

parseFloat :: Parser Joy
parseFloat = JFloat <$> Tok.float lexer

parseQuote :: Parser Joy
parseQuote = brackets p
  where p = JQuote <$> many joyVal

-- | Parse a simple word
parseWord :: Parser Joy
parseWord = JWord <$> identifier <* spaces

-- Parse a string literal
parseString :: Parser Joy
parseString = do
    char '"'
    s <- T.pack <$> many (noneOf "\"")
    char '"'
    return $ JString s

-- | ----------------------------------------

joyVal :: ParsecT T.Text () Identity Joy
joyVal = (try parseFloat <|> parseInteger)
     <|> parseString
     <|> parseQuote
     <|> parseWord

readExpr :: T.Text -> Either ParseError [Joy]
readExpr expr = parse (contents $ many joyVal) "<stdin>" expr
    where contents p = whitespace *> lexeme p <* eof
