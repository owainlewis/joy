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
  parseTrue  = string "true" *> notFollowedBy identChar *> pure (Literal $ Boolean True)
  parseFalse = string "false" *> notFollowedBy identChar *> pure (Literal $ Boolean False)

parseChar :: Parser Joy
parseChar = Lexer.lexeme $ do
  char '\''
  c <- noneOf "'"
  char '\''
  return $ Literal . Char $ c

parseInteger :: Parser Joy
parseInteger = Lexer.lexeme $ do
  sign <- option "" (string "-")
  digits <- many1 digit
  notFollowedBy (char '.')
  return $ Literal . Integer . read $ sign ++ digits

parseFloat :: Parser Joy
parseFloat = Lexer.lexeme $ do
  sign <- option "" (string "-")
  whole <- many1 digit
  char '.'
  fractional <- many1 digit
  return $ Literal . Float . read $ sign ++ whole ++ "." ++ fractional

-- String
parseString :: Parser Joy
parseString = Lexer.lexeme $ do
  char '"'
  str <- many (noneOf "\"")
  char '"'
  return $ Literal . String $ str

-- List
parseList :: Parser Joy
parseList = Lexer.brackets (Lexer.lexeme p) where p = List <$> many joyVal

-- Identifier (includes symbolic operators like +, -, *, /, etc.)
parseIdentifier :: Parser Joy
parseIdentifier = Literal . Identifier <$> Lexer.lexeme (wordIdent <|> symbolIdent)

identChar :: Parser Char
identChar = alphaNum <|> oneOf "-_?"

-- Standard word identifiers.
wordIdent :: Parser String
wordIdent = do
  first <- letter
  rest <- many identChar
  return (first : rest)

-- Symbolic operators.
symbolIdent :: Parser String
symbolIdent = many1 (oneOf "+-*/<>=!&|%^~")

parseDefinition :: Parser Joy
parseDefinition = do
  k <- Lexer.lexeme wordIdent
  string "=="
  Lexer.whitespace
  forms <- many joyVal
  return $ Definition k forms

parseDefinitionList :: Parser Joy
parseDefinitionList = do
  string "DEFINE"
  Lexer.whitespace
  forms <- sepEndBy parseDefinition (Lexer.lexeme $ char ';')
  Lexer.whitespace
  Lexer.lexeme $ char '.'
  return $ DefinitionList forms

-- | Parser
joyVal :: Parser Joy
joyVal =
  parseString
    <|> parseList
    <|> try parseDefinitionList
    <|> try parseDefinition
    <|> try parseFloat
    <|> try parseInteger
    <|> try parseBoolean
    <|> parseChar
    <|> parseIdentifier


readJoyExpr :: String -> Either ParseError [Joy]
readJoyExpr expr = parse (contents $ many joyVal) "<stdin>" expr
  where contents p = Lexer.whitespace *> Lexer.lexeme p <* eof

readJoyFile :: FilePath -> IO (Either ParseError [Joy])
readJoyFile f = readJoyExpr <$> IO.readFile f
