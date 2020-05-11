{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.Parser
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- Parser for the Joy programming language
----------------------------------------------------------------------------
module Language.Joy.Parser
  ( readJoyExpr
  , readJoyFile
  )
where

import           Language.Joy.AST
import qualified Language.Joy.Lexer            as Lexer

import qualified System.IO                     as IO
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

-- Bool
parseBoolean :: Parser Joy
parseBoolean = Lexer.lexeme $ try parseTrue <|> parseFalse
 where
  parseTrue  = (\_ -> Literal $ Boolean True) <$> string "true"
  parseFalse = (\_ -> Literal $ Boolean False) <$> string "false"

parseChar :: Parser Joy
parseChar = do
  char '\''
  c <- Lexer.lexeme $ anyChar
  optional $ char '\''
  return $ Literal . Char $ c

parseInteger :: Parser Joy
parseInteger = Literal . Integer <$> Lexer.integer

parseFloat :: Parser Joy
parseFloat = Literal . Float <$> Lexer.float

-- String
parseString :: Parser Joy
parseString = do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ Literal . String $ str

-- List
parseList :: Parser Joy
parseList = Lexer.brackets (Lexer.lexeme p) where p = List <$> many joyVal

-- Identifier
parseIdentifier :: Parser Joy
parseIdentifier = Literal . Identifier <$> Lexer.lexeme (many1 letter)

parseDefinition :: Parser Joy
parseDefinition = do
  k <- Lexer.lexeme (many1 letter)
  string "=="
  Lexer.whitespace
  forms <- many joyVal
  --char ';'
  return $ Definition k forms

parseDefinitionList :: Parser Joy
parseDefinitionList = do
  string "DEFINE"
  Lexer.whitespace
  forms <- sepBy parseDefinition (Lexer.lexeme $ char ';')
  Lexer.whitespace
  char '.'
  return $ DefinitionList forms

-- | Parser
joyVal :: Parser Joy
joyVal =
  parseString
    <|> parseList
    <|> (try parseFloat <|> parseInteger)
    <|> (try parseBoolean)
    <|> parseChar
    <|> parseDefinitionList
    <|> parseIdentifier


readJoyExpr :: String -> Either ParseError [Joy]
readJoyExpr expr = parse (contents $ many joyVal) "<stdin>" expr
  where contents p = Lexer.whitespace *> Lexer.lexeme p <* eof

readJoyFile :: FilePath -> IO (Either ParseError [Joy])
readJoyFile f = readJoyExpr <$> IO.readFile f
