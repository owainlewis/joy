module Main ( main ) where

import           Control.Monad (unless)
import qualified Language.Joy  as Joy
import           System.IO     (getLine, hFlush, stdout)

readOnce :: IO String
readOnce = putStr "JOY> "
        >> hFlush stdout
        >> getLine

main :: IO ()
main = do
    input <- readOnce
    unless (input == "quit") $
      case Joy.runJoy input of
        Left e      -> putStrLn e >> main
        Right exprs -> (putStrLn . show $ exprs) >> main
