{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Joy.Parser () where

import           Data.Functor.Identity                  (Identity)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Tok

import           Language.Joy.Core                      (Joy (..))

joyDef :: Tok.LanguageDef st
joyDef = emptyDef {
           Tok.commentStart    = "/*"
         , Tok.commentEnd      = "*/"
         , Tok.commentLine     = "//"
         , Tok.identStart      = letter
         , Tok.identLetter     = alphaNum
         , Tok.reservedNames   = [ "dup"
                                 ]
         , Tok.reservedOpNames = ["+", "-", "*", "/", ":="
                                 , "<", ">", "and", "or", "not"
                                 ]
         }

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser joyDef

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

brackets = Tok.brackets lexer

integer :: Parser Integer
integer = Tok.integer lexer

decimal :: Parser Joy
decimal = JFloat <$> Tok.float lexer

quotation :: Parser Joy
quotation = brackets p
  where p = JQuote <$> many parseExpr

-- Parse a string literal
parseString :: Parser Joy
parseString = do
    char '"'
    s <- many (noneOf "\"")
    char '"'
    return $ JString s

parseExpr = quotation

-- Bind whitespace later
parseJoy :: Parser a -> String -> a
parseJoy p str =  case parse p "" str of
    Left err  -> error $ "parse error at " ++ (show err)
    Right val -> val

test = parseJoy parseExpr
    where parseExpr = parseString
