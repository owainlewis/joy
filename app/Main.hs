{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Control.Monad                  ( unless )
import qualified Data.Map.Strict               as M
import qualified Data.Text                     as T
import qualified Language.Joy                  as Joy
import           Language.Joy.VirtualMachine    ( Joy(..)
                                                , Stack
                                                , VMError(..)
                                                , VMState(..)
                                                , runProgram
                                                , runProgramWithEnv
                                                )
import qualified Language.Joy.AST              as AST
import           Language.Joy.Parser            ( readJoyExpr )
import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )

-- | Format the stack for display
formatStack :: Stack -> String
formatStack [] = "[]"
formatStack xs = unwords $ map show (reverse xs)

-- | Format a Joy value for display
formatJoy :: Joy -> String
formatJoy = show

-- | Read a line of input with prompt
readOnce :: IO String
readOnce = putStr "joy> " >> hFlush stdout >> getLine

-- | Convert AST to VM program
astToProgram :: [AST.Joy] -> [Joy]
astToProgram = concatMap expandAst
  where
    expandAst :: AST.Joy -> [Joy]
    expandAst (AST.Literal (AST.Boolean b))    = [JBool b]
    expandAst (AST.Literal (AST.Char c))       = [JChar c]
    expandAst (AST.Literal (AST.Integer i))    = [JInt i]
    expandAst (AST.Literal (AST.Float f))      = [JFloat f]
    expandAst (AST.Literal (AST.String s))     = [JString (T.pack s)]
    expandAst (AST.Literal (AST.Identifier i)) = [JWord (T.pack i)]
    expandAst (AST.List js)                    = [JQuote (concatMap expandAst js)]
    expandAst (AST.Definition name body)       =
      [JQuote (concatMap expandAst body), JWord (T.pack name), JWord "define"]
    expandAst (AST.DefinitionList defs)        = concatMap expandAst defs

-- | Main REPL loop
repl :: IO ()
repl = replWithEnv M.empty

-- | REPL with persistent environment
replWithEnv :: Joy.Env -> IO ()
replWithEnv env = do
  input <- readOnce
  unless (input == "quit" || input == ":q" || input == ":quit") $ do
    case input of
      -- Empty input
      "" -> replWithEnv env

      -- Help command
      ":help" -> do
        putStrLn ""
        putStrLn "Joy Interpreter - A concatenative stack-based language"
        putStrLn ""
        putStrLn "Commands:"
        putStrLn "  :help          Show this help message"
        putStrLn "  :load <file>   Load and execute a Joy file"
        putStrLn "  :env           Show defined words"
        putStrLn "  :clear         Clear all definitions"
        putStrLn "  :quit, :q      Exit the REPL"
        putStrLn ""
        putStrLn "Examples:"
        putStrLn "  1 2 +          => 3"
        putStrLn "  [1 2 3] dup    => [1 2 3] [1 2 3]"
        putStrLn "  5 dup *        => 25"
        putStrLn "  [1 2 3] [dup *] map  => [1 4 9]"
        putStrLn ""
        putStrLn "Defining words:"
        putStrLn "  [dup *] square define"
        putStrLn "  5 square       => 25"
        putStrLn ""
        replWithEnv env

      -- Show environment
      ":env" -> do
        if M.null env
          then putStrLn "No definitions."
          else do
            putStrLn "Defined words:"
            mapM_ (\(k, v) -> putStrLn $ "  " ++ T.unpack k ++ " = " ++ show v) (M.toList env)
        replWithEnv env

      -- Clear environment
      ":clear" -> do
        putStrLn "Definitions cleared."
        replWithEnv M.empty

      -- Load file
      (':':'l':'o':'a':'d':' ':filename) -> do
        result <- Joy.runJoyFile (trim filename)
        case result of
          Left err -> do
            putStrLn $ "Error: " ++ err
            replWithEnv env
          Right stack -> do
            putStrLn $ "=> " ++ formatStack stack
            replWithEnv env

      -- Normal Joy execution
      _ -> do
        case readJoyExpr input of
          Left err -> do
            putStrLn $ "Parse error: " ++ show err
            replWithEnv env
          Right ast -> do
            let program = astToProgram ast
            case runProgramWithEnv env program of
              Left err -> do
                putStrLn $ "Error: " ++ show err
                replWithEnv env
              Right stack -> do
                putStrLn $ "=> " ++ formatStack stack
                -- Note: In a full implementation, we'd extract new definitions
                -- from the execution and add them to env
                replWithEnv env

-- | Trim whitespace from string
trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

-- | Run a file and print results
runFile :: FilePath -> IO ()
runFile path = do
  result <- Joy.runJoyFile path
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right stack -> do
      putStrLn $ "Result: " ++ formatStack stack

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Run a file if provided
    [filename] -> runFile filename

    -- Interactive mode
    [] -> do
      putStrLn "Joy Interpreter v0.2.0"
      putStrLn "Type :help for help, :quit to exit"
      putStrLn ""
      repl

    -- Multiple arguments - try to evaluate as Joy code
    _ -> do
      let code = unwords args
      case Joy.runJoy code of
        Left err -> putStrLn $ "Error: " ++ err
        Right stack -> putStrLn $ formatStack stack
