{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
  , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
  , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
  }

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

brackets = Tok.brackets lexer

parseInteger :: Parser Joy
parseInteger = JInt <$> Tok.integer lexer

parseFloat :: Parser Joy
parseFloat = JFloat <$> Tok.float lexer

parseQuote :: Parser Joy
parseQuote = brackets p
  where p = JQuote <$> many joyVal

-- Parse a string literal
parseString :: Parser Joy
parseString = do
    char '"'
    s <- many (noneOf "\"")
    char '"'
    return $ JString s

joyVal :: ParsecT T.Text () Identity Joy
joyVal = (try parseFloat <|> parseInteger)
     <|> parseString
     <|> parseQuote

readExpr :: T.Text -> Either ParseError Joy
readExpr expr = parse (contents joyVal) "<stdin>" expr
    where contents p = whitespace *> lexeme p <* eof
