module Main ( main ) where

import           Control.Monad (unless)
import           System.IO     (getLine, hFlush, stdout)

readOnce :: IO String
readOnce = putStr "JOY> "
        >> hFlush stdout
        >> getLine

main :: IO ()
main = do
    input <- readOnce
    unless (input == "quit")
      $ putStrLn input >> main
