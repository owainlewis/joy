{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Joy.Parser () where

import           Data.Functor.Identity                  (Identity)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Tok

joyDef :: Tok.LanguageDef st
joyDef = Tok.LanguageDef
                { Tok.commentStart   = ""
                , Tok.commentEnd     = ""
                , Tok.commentLine    = "--"
                , Tok.nestedComments = True
                , Tok.identStart     = letter
                , Tok.identLetter    = alphaNum <|> oneOf "_'"
                , Tok.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                , Tok.opStart        = Tok.opLetter joyDef
                , Tok.reservedOpNames= []
                , Tok.reservedNames  = []
                , Tok.caseSensitive  = True
                }
    where ops = []

lexer :: Tok.GenTokenParser String u Identity
lexer = Tok.makeTokenParser joyDef

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

joyParser _ = ()

parseJoy :: Parser a -> String -> a
parseJoy p str =  case parse p "" str of
    Left err  -> error $ "parse error at " ++ (show err)
    Right val -> val
