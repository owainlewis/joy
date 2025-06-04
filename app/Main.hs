module Main
  ( main
  )
where

import           Control.Monad                  ( unless )
import qualified Language.Joy                  as Joy
import           System.Environment             ( getArgs )
import           System.IO                      ( getLine
                                                , hFlush
                                                , stdout
                                                )

readOnce :: IO String
readOnce = putStr "JOY> " >> hFlush stdout >> getLine

repl :: IO ()
repl = do
  input <- readOnce
  unless (input == "quit") $ do
    case input of
      -- Special commands
      ":help" -> do
        putStrLn "Joy Interpreter REPL"
        putStrLn "Commands:"
        putStrLn "  :help - Show this help message"
        putStrLn "  :load <filename> - Load and execute a Joy file"
        putStrLn "  quit - Exit the REPL"
        repl
      (':':'l':'o':'a':'d':' ':filename) -> do
        result <- Joy.runJoyFile filename
        case result of
          Left err -> putStrLn $ "Error loading file: " ++ err
          Right exprs -> putStrLn $ "Loaded: " ++ show exprs
        repl
      -- Normal Joy execution
      _ -> do
        -- Parse first
        case Joy.runJoy input of
          Left e -> do
            putStrLn $ "Parse error: " ++ e
            repl
          Right exprs -> do
            putStrLn $ "Parsed: " ++ show exprs
            -- Then evaluate
            result <- Joy.evalJoy input
            case result of
              Left err -> putStrLn $ "Execution error: " ++ show err
              Right _ -> putStrLn "Executed successfully"
            repl

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Run a file if provided
    [filename] -> do
      result <- Joy.runJoyFile filename
      case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right exprs -> do
          putStrLn $ "Parsed: " ++ show exprs
          evalResult <- Joy.evalJoy =<< readFile filename
          case evalResult of
            Left err -> putStrLn $ "Execution error: " ++ show err
            Right _ -> putStrLn "Executed successfully"
    -- Otherwise start REPL
    _ -> do
      putStrLn "Joy Interpreter"
      putStrLn "Type :help for commands, or 'quit' to exit"
      repl
