{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Joy.VirtualMachine
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
--
-- A stack-based virtual machine for executing Joy programs.
-- Joy is a concatenative language where programs are functions that
-- transform stacks into stacks.
--
----------------------------------------------------------------------------
module Language.Joy.VirtualMachine
  ( -- * Types
    Joy(..)
  , Stack
  , Env
  , VM
  , VMState(..)
  , VMError(..)
    -- * Running programs
  , runProgram
  , runProgramWithEnv
  , runProgramStateWithEnv
  , evalJoy
  , evalJoyList
    -- * Stack operations (for testing)
  , emptyState
  , getStack
  ) where

import           Control.Monad.Except
import           Control.Monad        (filterM, foldM, replicateM_)
import           Control.Monad.State
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import           Data.Text              (Text)
import           Data.List              (intercalate)

-----------------------------------------------------------------------------
-- Core Types
-----------------------------------------------------------------------------

-- | Joy values - the core data types of the language
data Joy
  = JInt Integer          -- ^ Integer literal
  | JFloat Double         -- ^ Floating point literal
  | JBool Bool            -- ^ Boolean literal
  | JChar Char            -- ^ Character literal
  | JString Text          -- ^ String literal
  | JWord Text            -- ^ Word/identifier (to be looked up)
  | JQuote [Joy]          -- ^ Quotation (list/program)
  deriving (Eq, Ord)

instance Show Joy where
  show (JInt n)    = show n
  show (JFloat f)  = show f
  show (JBool b)   = if b then "true" else "false"
  show (JChar c)   = show c
  show (JString s) = show s
  show (JWord w)   = T.unpack w
  show (JQuote js) = "[" ++ unwords (map show js) ++ "]"

-- | The stack is a list of Joy values (head is top)
type Stack = [Joy]

-- | Environment maps names to quotations (definitions)
type Env = Map Text [Joy]

-- | Virtual machine state
data VMState = VMState
  { vmStack :: Stack    -- ^ The data stack
  , vmEnv   :: Env      -- ^ User-defined words
  } deriving (Show, Eq)

-- | VM errors
data VMError
  = StackUnderflow Text Int Int    -- ^ operation, expected, actual
  | TypeError Text Text Text       -- ^ operation, expected, actual
  | UndefinedWord Text             -- ^ undefined word
  | DivisionByZero                 -- ^ division by zero
  | EmptyQuotation Text            -- ^ operation requiring non-empty quotation
  | RuntimeError Text              -- ^ generic runtime error
  deriving (Eq)

instance Show VMError where
  show (StackUnderflow op expected actual) =
    "Stack underflow in '" ++ T.unpack op ++ "': expected " ++
    show expected ++ " elements, got " ++ show actual
  show (TypeError op expected actual) =
    "Type error in '" ++ T.unpack op ++ "': expected " ++
    T.unpack expected ++ ", got " ++ T.unpack actual
  show (UndefinedWord w) =
    "Undefined word: " ++ T.unpack w
  show DivisionByZero =
    "Division by zero"
  show (EmptyQuotation op) =
    "Empty quotation in '" ++ T.unpack op ++ "'"
  show (RuntimeError msg) =
    "Runtime error: " ++ T.unpack msg

-- | The VM monad
type VM a = StateT VMState (Except VMError) a

-----------------------------------------------------------------------------
-- VM Helpers
-----------------------------------------------------------------------------

-- | Create empty VM state
emptyState :: VMState
emptyState = VMState [] M.empty

-- | Get current stack
getStack :: VM Stack
getStack = gets vmStack

-- | Push a value onto the stack
push :: Joy -> VM ()
push v = modify $ \s -> s { vmStack = v : vmStack s }

-- | Pop a value from the stack
pop :: Text -> VM Joy
pop op = do
  st <- gets vmStack
  case st of
    []     -> throwError $ StackUnderflow op 1 0
    (x:xs) -> modify (\s -> s { vmStack = xs }) >> return x

-- | Pop N values from the stack
popN :: Text -> Int -> VM [Joy]
popN op n = do
  st <- gets vmStack
  if length st < n
    then throwError $ StackUnderflow op n (length st)
    else do
      let (taken, rest) = splitAt n st
      modify (\s -> s { vmStack = rest })
      return taken

-- | Peek at top of stack without popping
peek :: Text -> VM Joy
peek op = do
  st <- gets vmStack
  case st of
    []    -> throwError $ StackUnderflow op 1 0
    (x:_) -> return x

-- | Add a definition to the environment
define :: Text -> [Joy] -> VM ()
define name body = modify $ \s -> s { vmEnv = M.insert name body (vmEnv s) }

-- | Look up a definition
lookupDef :: Text -> VM (Maybe [Joy])
lookupDef name = gets (M.lookup name . vmEnv)

-----------------------------------------------------------------------------
-- Evaluation
-----------------------------------------------------------------------------

-- | Evaluate a single Joy value
evalJoy :: Joy -> VM ()
evalJoy val = case val of
  -- Literals just push themselves onto the stack
  JInt _    -> push val
  JFloat _  -> push val
  JBool _   -> push val
  JChar _   -> push val
  JString _ -> push val
  JQuote _  -> push val

  -- Words need to be looked up and executed
  JWord w   -> evalWord w

-- | Evaluate a list of Joy values (a program)
evalJoyList :: [Joy] -> VM ()
evalJoyList [] = return ()
evalJoyList (JQuote body : JWord name : JWord "define" : rest) = do
  define name body
  evalJoyList rest
evalJoyList (val : rest) = do
  evalJoy val
  evalJoyList rest

-- | Evaluate a word - either a primitive or user-defined
evalWord :: Text -> VM ()
evalWord w = do
  -- First check if it's a primitive
  case M.lookup w primitives of
    Just prim -> prim
    Nothing -> do
      -- Then check user definitions
      def <- lookupDef w
      case def of
        Just body -> evalJoyList body
        Nothing   -> throwError $ UndefinedWord w

-----------------------------------------------------------------------------
-- Primitives
-----------------------------------------------------------------------------

-- | All built-in primitives
primitives :: Map Text (VM ())
primitives = M.fromList
  -- Stack operations
  [ ("dup",     opDup)
  , ("pop",     opPop)
  , ("swap",    opSwap)
  , ("rollup",  opRollup)
  , ("rolldown", opRolldown)
  , ("rotate",  opRotate)
  , ("dupd",    opDupd)
  , ("swapd",   opSwapd)
  , ("popd",    opPopd)
  , ("id",      return ())  -- identity, do nothing
  , ("newstack", modify $ \s -> s { vmStack = [] })
  , ("stack",   opStack)
  , ("unstack", opUnstack)

  -- Arithmetic
  , ("+",       opAdd)
  , ("-",       opSub)
  , ("*",       opMul)
  , ("/",       opDiv)
  , ("%",       opMod)
  , ("rem",     opMod)
  , ("div",     opIntDiv)
  , ("abs",     opAbs)
  , ("neg",     opNeg)
  , ("sign",    opSign)
  , ("max",     opMax)
  , ("min",     opMin)
  , ("succ",    opSucc)
  , ("pred",    opPred)

  -- Comparison
  , ("<",       opLt)
  , (">",       opGt)
  , ("<=",      opLe)
  , (">=",      opGe)
  , ("=",       opEq)
  , ("!=",      opNe)
  , ("<>",      opNe)

  -- Boolean
  , ("and",     opAnd)
  , ("or",      opOr)
  , ("not",     opNot)
  , ("xor",     opXor)

  -- List operations
  , ("cons",    opCons)
  , ("swons",   opSwons)
  , ("first",   opFirst)
  , ("rest",    opRest)
  , ("uncons",  opUncons)
  , ("unswons", opUnswons)
  , ("concat",  opConcat)
  , ("size",    opSize)
  , ("null",    opNull)
  , ("small",   opSmall)
  , ("reverse", opReverse)
  , ("at",      opAt)
  , ("of",      opOf)
  , ("drop",    opDrop)
  , ("take",    opTake)

  -- Quotation execution
  , ("i",       opI)
  , ("x",       opX)
  , ("dip",     opDip)
  , ("dipd",    opDipd)
  , ("dipdd",   opDipdd)
  , ("app1",    opApp1)
  , ("app2",    opApp2)
  , ("nullary", opNullary)
  , ("unary",   opUnary)
  , ("binary",  opBinary)
  , ("ternary", opTernary)

  -- Conditionals
  , ("ifte",    opIfte)
  , ("cond",    opCond)
  , ("choice",  opChoice)
  , ("branch",  opBranch)

  -- List processing combinators
  , ("map",     opMap)
  , ("filter",  opFilter)
  , ("fold",    opFold)
  , ("step",    opStep)
  , ("split",   opSplit)
  , ("times",   opTimes)

  -- Recursion combinators
  , ("linrec",  opLinrec)
  , ("primrec", opPrimrec)
  , ("tailrec", opTailrec)
  , ("genrec",  opGenrec)
  , ("binrec",  opBinrec)

  -- Type predicates
  , ("integer?", opIsInteger)
  , ("float?",   opIsFloat)
  , ("number?",  opIsNumber)
  , ("char?",    opIsChar)
  , ("string?",  opIsString)
  , ("list?",    opIsList)
  , ("leaf?",    opIsLeaf)
  , ("logical?", opIsLogical)

  -- Type conversions
  , ("ord",     opOrd)
  , ("chr",     opChr)
  , ("strtol",  opStrtol)

  -- I/O
  , ("put",     opPut)
  , ("putch",   opPutch)
  , ("print",   opPrint)

  -- Miscellaneous
  , ("unit",    opUnit)
  , ("pair",    opPair)
  , ("unpair",  opUnpair)
  , ("infra",   opInfra)
  , ("cleave",  opCleave)

  -- Definition
  , ("define",  opDefine)
  ]

-----------------------------------------------------------------------------
-- Stack Operations
-----------------------------------------------------------------------------

-- | dup: X -> X X
opDup :: VM ()
opDup = do
  x <- pop "dup"
  push x >> push x

-- | pop: X ->
opPop :: VM ()
opPop = pop "pop" >> return ()

-- | swap: X Y -> Y X
opSwap :: VM ()
opSwap = do
  y <- pop "swap"
  x <- pop "swap"
  push y >> push x

-- | rollup: X Y Z -> Z X Y
opRollup :: VM ()
opRollup = do
  z <- pop "rollup"
  y <- pop "rollup"
  x <- pop "rollup"
  push z >> push x >> push y

-- | rolldown: X Y Z -> Y Z X
opRolldown :: VM ()
opRolldown = do
  z <- pop "rolldown"
  y <- pop "rolldown"
  x <- pop "rolldown"
  push y >> push z >> push x

-- | rotate: X Y Z -> Z Y X
opRotate :: VM ()
opRotate = do
  z <- pop "rotate"
  y <- pop "rotate"
  x <- pop "rotate"
  push z >> push y >> push x

-- | dupd: Y Z -> Y Y Z (dup second element)
opDupd :: VM ()
opDupd = do
  z <- pop "dupd"
  y <- pop "dupd"
  push y >> push y >> push z

-- | swapd: X Y Z -> Y X Z (swap under top)
opSwapd :: VM ()
opSwapd = do
  z <- pop "swapd"
  y <- pop "swapd"
  x <- pop "swapd"
  push y >> push x >> push z

-- | popd: Y Z -> Z (pop second element)
opPopd :: VM ()
opPopd = do
  z <- pop "popd"
  _ <- pop "popd"
  push z

-- | stack: ... -> [...]
opStack :: VM ()
opStack = do
  st <- gets vmStack
  push (JQuote st)

-- | unstack: [X Y Z] -> X Y Z
opUnstack :: VM ()
opUnstack = do
  q <- pop "unstack"
  case q of
    JQuote xs -> modify $ \s -> s { vmStack = xs }
    _ -> throwError $ TypeError "unstack" "quotation" (typeOf q)

-----------------------------------------------------------------------------
-- Arithmetic Operations
-----------------------------------------------------------------------------

-- | Helper: apply binary numeric operation
binaryNumOp :: Text -> (Integer -> Integer -> Integer)
            -> (Double -> Double -> Double) -> VM ()
binaryNumOp op intOp floatOp = do
  b <- pop op
  a <- pop op
  case (a, b) of
    (JInt x, JInt y)     -> push $ JInt (intOp x y)
    (JFloat x, JFloat y) -> push $ JFloat (floatOp x y)
    (JInt x, JFloat y)   -> push $ JFloat (floatOp (fromIntegral x) y)
    (JFloat x, JInt y)   -> push $ JFloat (floatOp x (fromIntegral y))
    _ -> throwError $ TypeError op "number" (typeOf a <> " and " <> typeOf b)

opAdd, opSub, opMul :: VM ()
opAdd = binaryNumOp "+" (+) (+)
opSub = binaryNumOp "-" (-) (-)
opMul = binaryNumOp "*" (*) (*)

opDiv :: VM ()
opDiv = do
  b <- pop "/"
  a <- pop "/"
  case (a, b) of
    (_, JInt 0)   -> throwError DivisionByZero
    (_, JFloat 0) -> throwError DivisionByZero
    (JInt x, JInt y)     -> push $ JFloat (fromIntegral x / fromIntegral y)
    (JFloat x, JFloat y) -> push $ JFloat (x / y)
    (JInt x, JFloat y)   -> push $ JFloat (fromIntegral x / y)
    (JFloat x, JInt y)   -> push $ JFloat (x / fromIntegral y)
    _ -> throwError $ TypeError "/" "number" (typeOf a <> " and " <> typeOf b)

opIntDiv :: VM ()
opIntDiv = do
  b <- pop "div"
  a <- pop "div"
  case (a, b) of
    (_, JInt 0) -> throwError DivisionByZero
    (JInt x, JInt y) -> push $ JInt (x `div` y)
    _ -> throwError $ TypeError "div" "integer" (typeOf a <> " and " <> typeOf b)

opMod :: VM ()
opMod = do
  b <- pop "%"
  a <- pop "%"
  case (a, b) of
    (_, JInt 0) -> throwError DivisionByZero
    (JInt x, JInt y) -> push $ JInt (x `mod` y)
    _ -> throwError $ TypeError "%" "integer" (typeOf a <> " and " <> typeOf b)

opAbs :: VM ()
opAbs = do
  a <- pop "abs"
  case a of
    JInt x   -> push $ JInt (abs x)
    JFloat x -> push $ JFloat (abs x)
    _ -> throwError $ TypeError "abs" "number" (typeOf a)

opNeg :: VM ()
opNeg = do
  a <- pop "neg"
  case a of
    JInt x   -> push $ JInt (negate x)
    JFloat x -> push $ JFloat (negate x)
    _ -> throwError $ TypeError "neg" "number" (typeOf a)

opSign :: VM ()
opSign = do
  a <- pop "sign"
  case a of
    JInt x   -> push $ JInt (signum x)
    JFloat x -> push $ JInt (truncate $ signum x)
    _ -> throwError $ TypeError "sign" "number" (typeOf a)

opMax, opMin :: VM ()
opMax = binaryNumOp "max" max max
opMin = binaryNumOp "min" min min

opSucc, opPred :: VM ()
opSucc = do
  a <- pop "succ"
  case a of
    JInt x -> push $ JInt (x + 1)
    JChar c -> push $ JChar (succ c)
    _ -> throwError $ TypeError "succ" "integer or char" (typeOf a)

opPred = do
  a <- pop "pred"
  case a of
    JInt x -> push $ JInt (x - 1)
    JChar c -> push $ JChar (pred c)
    _ -> throwError $ TypeError "pred" "integer or char" (typeOf a)

-----------------------------------------------------------------------------
-- Comparison Operations
-----------------------------------------------------------------------------

-- | Helper: apply comparison operation
compareOp :: Text -> (Integer -> Integer -> Bool)
          -> (Double -> Double -> Bool) -> VM ()
compareOp op intCmp floatCmp = do
  b <- pop op
  a <- pop op
  case (a, b) of
    (JInt x, JInt y)     -> push $ JBool (intCmp x y)
    (JFloat x, JFloat y) -> push $ JBool (floatCmp x y)
    (JInt x, JFloat y)   -> push $ JBool (floatCmp (fromIntegral x) y)
    (JFloat x, JInt y)   -> push $ JBool (floatCmp x (fromIntegral y))
    (JChar x, JChar y)   -> push $ JBool (intCmp (fromIntegral $ fromEnum x) (fromIntegral $ fromEnum y))
    (JString x, JString y) -> push $ JBool (compareText x y op)
    _ -> throwError $ TypeError op "comparable" (typeOf a <> " and " <> typeOf b)
  where
    compareText x y "<"  = x < y
    compareText x y ">"  = x > y
    compareText x y "<=" = x <= y
    compareText x y ">=" = x >= y
    compareText _ _ _    = False

opLt, opGt, opLe, opGe :: VM ()
opLt = compareOp "<" (<) (<)
opGt = compareOp ">" (>) (>)
opLe = compareOp "<=" (<=) (<=)
opGe = compareOp ">=" (>=) (>=)

opEq, opNe :: VM ()
opEq = do
  b <- pop "="
  a <- pop "="
  push $ JBool (a == b)

opNe = do
  b <- pop "!="
  a <- pop "!="
  push $ JBool (a /= b)

-----------------------------------------------------------------------------
-- Boolean Operations
-----------------------------------------------------------------------------

opAnd, opOr, opNot, opXor :: VM ()
opAnd = do
  b <- pop "and"
  a <- pop "and"
  case (a, b) of
    (JBool x, JBool y) -> push $ JBool (x && y)
    _ -> throwError $ TypeError "and" "boolean" (typeOf a <> " and " <> typeOf b)

opOr = do
  b <- pop "or"
  a <- pop "or"
  case (a, b) of
    (JBool x, JBool y) -> push $ JBool (x || y)
    _ -> throwError $ TypeError "or" "boolean" (typeOf a <> " and " <> typeOf b)

opNot = do
  a <- pop "not"
  case a of
    JBool x -> push $ JBool (not x)
    _ -> throwError $ TypeError "not" "boolean" (typeOf a)

opXor = do
  b <- pop "xor"
  a <- pop "xor"
  case (a, b) of
    (JBool x, JBool y) -> push $ JBool (x /= y)  -- xor is inequality for bools
    _ -> throwError $ TypeError "xor" "boolean" (typeOf a <> " and " <> typeOf b)

-----------------------------------------------------------------------------
-- List Operations
-----------------------------------------------------------------------------

-- | cons: X [Y...] -> [X Y...]
opCons :: VM ()
opCons = do
  q <- pop "cons"
  x <- pop "cons"
  case q of
    JQuote ys -> push $ JQuote (x : ys)
    _ -> throwError $ TypeError "cons" "quotation" (typeOf q)

-- | swons: [Y...] X -> [X Y...] (swap then cons)
opSwons :: VM ()
opSwons = opSwap >> opCons

-- | first: [X Y...] -> X
opFirst :: VM ()
opFirst = do
  q <- pop "first"
  case q of
    JQuote (x:_) -> push x
    JQuote []    -> throwError $ EmptyQuotation "first"
    _ -> throwError $ TypeError "first" "quotation" (typeOf q)

-- | rest: [X Y...] -> [Y...]
opRest :: VM ()
opRest = do
  q <- pop "rest"
  case q of
    JQuote (_:xs) -> push $ JQuote xs
    JQuote []     -> throwError $ EmptyQuotation "rest"
    _ -> throwError $ TypeError "rest" "quotation" (typeOf q)

-- | uncons: [X Y...] -> X [Y...]
opUncons :: VM ()
opUncons = do
  q <- pop "uncons"
  case q of
    JQuote (x:xs) -> push x >> push (JQuote xs)
    JQuote []     -> throwError $ EmptyQuotation "uncons"
    _ -> throwError $ TypeError "uncons" "quotation" (typeOf q)

-- | unswons: [X Y...] -> [Y...] X
opUnswons :: VM ()
opUnswons = opUncons >> opSwap

-- | concat: [X...] [Y...] -> [X... Y...]
opConcat :: VM ()
opConcat = do
  b <- pop "concat"
  a <- pop "concat"
  case (a, b) of
    (JQuote xs, JQuote ys) -> push $ JQuote (xs ++ ys)
    (JString xs, JString ys) -> push $ JString (xs <> ys)
    _ -> throwError $ TypeError "concat" "quotation or string" (typeOf a <> " and " <> typeOf b)

-- | size: [X...] -> N
opSize :: VM ()
opSize = do
  a <- pop "size"
  case a of
    JQuote xs  -> push $ JInt (fromIntegral $ length xs)
    JString xs -> push $ JInt (fromIntegral $ T.length xs)
    _ -> throwError $ TypeError "size" "quotation or string" (typeOf a)

-- | null: [X...] -> Bool (true if empty)
opNull :: VM ()
opNull = do
  a <- pop "null"
  case a of
    JQuote xs  -> push $ JBool (null xs)
    JString xs -> push $ JBool (T.null xs)
    JInt n     -> push $ JBool (n == 0)
    _ -> throwError $ TypeError "null" "quotation, string, or integer" (typeOf a)

-- | small: X -> Bool (true if 0, 1, empty, or singleton)
opSmall :: VM ()
opSmall = do
  a <- pop "small"
  case a of
    JQuote xs  -> push $ JBool (length xs <= 1)
    JString xs -> push $ JBool (T.length xs <= 1)
    JInt n     -> push $ JBool (n == 0 || n == 1)
    _ -> throwError $ TypeError "small" "quotation, string, or integer" (typeOf a)

-- | reverse: [X Y Z] -> [Z Y X]
opReverse :: VM ()
opReverse = do
  a <- pop "reverse"
  case a of
    JQuote xs  -> push $ JQuote (reverse xs)
    JString xs -> push $ JString (T.reverse xs)
    _ -> throwError $ TypeError "reverse" "quotation or string" (typeOf a)

-- | at: [X Y Z] N -> element at index N
opAt :: VM ()
opAt = do
  n <- pop "at"
  a <- pop "at"
  case (a, n) of
    (JQuote xs, JInt i) ->
      if i >= 0 && i < fromIntegral (length xs)
        then push $ xs !! fromIntegral i
        else throwError $ RuntimeError "index out of bounds"
    (JString xs, JInt i) ->
      if i >= 0 && i < fromIntegral (T.length xs)
        then push $ JChar (T.index xs (fromIntegral i))
        else throwError $ RuntimeError "index out of bounds"
    _ -> throwError $ TypeError "at" "quotation/string and integer" (typeOf a <> " and " <> typeOf n)

-- | of: N [X Y Z] -> element at index N (like at but args swapped)
opOf :: VM ()
opOf = opSwap >> opAt

-- | drop: [X Y Z] N -> drop first N elements
opDrop :: VM ()
opDrop = do
  n <- pop "drop"
  a <- pop "drop"
  case (a, n) of
    (JQuote xs, JInt i)  -> push $ JQuote (drop (fromIntegral i) xs)
    (JString xs, JInt i) -> push $ JString (T.drop (fromIntegral i) xs)
    _ -> throwError $ TypeError "drop" "quotation/string and integer" (typeOf a <> " and " <> typeOf n)

-- | take: [X Y Z] N -> take first N elements
opTake :: VM ()
opTake = do
  n <- pop "take"
  a <- pop "take"
  case (a, n) of
    (JQuote xs, JInt i)  -> push $ JQuote (take (fromIntegral i) xs)
    (JString xs, JInt i) -> push $ JString (T.take (fromIntegral i) xs)
    _ -> throwError $ TypeError "take" "quotation/string and integer" (typeOf a <> " and " <> typeOf n)

-----------------------------------------------------------------------------
-- Quotation Execution (Combinators)
-----------------------------------------------------------------------------

-- | i: [P] -> execute P
opI :: VM ()
opI = do
  q <- pop "i"
  case q of
    JQuote body -> evalJoyList body
    _ -> throwError $ TypeError "i" "quotation" (typeOf q)

-- | x: [P] -> [P] P (dup then i)
opX :: VM ()
opX = do
  q <- pop "x"
  case q of
    JQuote body -> do
      evalJoyList body
      result <- pop "x"
      push q
      push result
    _ -> throwError $ TypeError "x" "quotation" (typeOf q)

-- | dip: X [P] -> P X (execute P under X)
opDip :: VM ()
opDip = do
  q <- pop "dip"
  x <- pop "dip"
  case q of
    JQuote body -> do
      evalJoyList body
      push x
    _ -> throwError $ TypeError "dip" "quotation" (typeOf q)

-- | dipd: Y X [P] -> P Y X (dip under two)
opDipd :: VM ()
opDipd = do
  q <- pop "dipd"
  x <- pop "dipd"
  y <- pop "dipd"
  case q of
    JQuote body -> do
      evalJoyList body
      push y >> push x
    _ -> throwError $ TypeError "dipd" "quotation" (typeOf q)

-- | dipdd: Z Y X [P] -> P Z Y X (dip under three)
opDipdd :: VM ()
opDipdd = do
  q <- pop "dipdd"
  x <- pop "dipdd"
  y <- pop "dipdd"
  z <- pop "dipdd"
  case q of
    JQuote body -> do
      evalJoyList body
      push z >> push y >> push x
    _ -> throwError $ TypeError "dipdd" "quotation" (typeOf q)

-- | app1: X [P] -> P(X) (apply preserving nothing)
opApp1 :: VM ()
opApp1 = opI

-- | app2: X Y [P] -> P(X) P(Y) (apply to two values)
opApp2 :: VM ()
opApp2 = do
  q <- pop "app2"
  y <- pop "app2"
  x <- pop "app2"
  case q of
    JQuote body -> do
      -- Apply to x
      push x
      evalJoyList body
      rx <- pop "app2"
      -- Apply to y
      push y
      evalJoyList body
      ry <- pop "app2"
      -- Push both results
      push rx >> push ry
    _ -> throwError $ TypeError "app2" "quotation" (typeOf q)

-- | nullary: [P] -> R (execute P, push single result, restore stack)
opNullary :: VM ()
opNullary = do
  q <- pop "nullary"
  savedStack <- gets vmStack
  case q of
    JQuote body -> do
      evalJoyList body
      result <- pop "nullary"
      modify $ \s -> s { vmStack = savedStack }
      push result
    _ -> throwError $ TypeError "nullary" "quotation" (typeOf q)

-- | unary: X [P] -> R (execute P on X, single result)
opUnary :: VM ()
opUnary = do
  q <- pop "unary"
  x <- pop "unary"
  savedStack <- gets vmStack
  case q of
    JQuote body -> do
      push x
      evalJoyList body
      result <- pop "unary"
      modify $ \s -> s { vmStack = savedStack }
      push result
    _ -> throwError $ TypeError "unary" "quotation" (typeOf q)

-- | binary: X Y [P] -> R (execute P on X Y, single result)
opBinary :: VM ()
opBinary = do
  q <- pop "binary"
  y <- pop "binary"
  x <- pop "binary"
  savedStack <- gets vmStack
  case q of
    JQuote body -> do
      push x >> push y
      evalJoyList body
      result <- pop "binary"
      modify $ \s -> s { vmStack = savedStack }
      push result
    _ -> throwError $ TypeError "binary" "quotation" (typeOf q)

-- | ternary: X Y Z [P] -> R
opTernary :: VM ()
opTernary = do
  q <- pop "ternary"
  z <- pop "ternary"
  y <- pop "ternary"
  x <- pop "ternary"
  savedStack <- gets vmStack
  case q of
    JQuote body -> do
      push x >> push y >> push z
      evalJoyList body
      result <- pop "ternary"
      modify $ \s -> s { vmStack = savedStack }
      push result
    _ -> throwError $ TypeError "ternary" "quotation" (typeOf q)

-----------------------------------------------------------------------------
-- Conditionals
-----------------------------------------------------------------------------

-- | ifte: [If] [Then] [Else] -> ...
opIfte :: VM ()
opIfte = do
  elseQ <- pop "ifte"
  thenQ <- pop "ifte"
  ifQ   <- pop "ifte"
  case (ifQ, thenQ, elseQ) of
    (JQuote ifBody, JQuote thenBody, JQuote elseBody) -> do
      -- Save stack for condition evaluation
      savedStack <- gets vmStack
      -- Evaluate condition
      evalJoyList ifBody
      cond <- pop "ifte"
      -- Restore stack (condition shouldn't affect it)
      modify $ \s -> s { vmStack = savedStack }
      -- Branch based on condition
      case cond of
        JBool True  -> evalJoyList thenBody
        JBool False -> evalJoyList elseBody
        _ -> throwError $ TypeError "ifte" "boolean" (typeOf cond)
    _ -> throwError $ TypeError "ifte" "three quotations" "non-quotation"

-- | cond: [[C1 T1] [C2 T2] ... [default]] -> ...
opCond :: VM ()
opCond = do
  cases <- pop "cond"
  case cases of
    JQuote clauses -> evalCond clauses
    _ -> throwError $ TypeError "cond" "quotation of clauses" (typeOf cases)
  where
    evalCond [] = throwError $ RuntimeError "cond: no matching clause"
    evalCond [JQuote defaultBody] = evalJoyList defaultBody
    evalCond (JQuote [cond, body]:rest) = do
      savedStack <- gets vmStack
      case cond of
        JQuote condBody -> evalJoyList condBody
        _ -> push cond  -- literal condition
      result <- pop "cond"
      modify $ \s -> s { vmStack = savedStack }
      case result of
        JBool True -> case body of
          JQuote bodyList -> evalJoyList bodyList
          _ -> push body
        JBool False -> evalCond rest
        _ -> throwError $ TypeError "cond" "boolean" (typeOf result)
    evalCond _ = throwError $ RuntimeError "cond: malformed clause"

-- | choice: B T F -> (if B then T else F)
opChoice :: VM ()
opChoice = do
  f <- pop "choice"
  t <- pop "choice"
  b <- pop "choice"
  case b of
    JBool True  -> push t
    JBool False -> push f
    _ -> throwError $ TypeError "choice" "boolean" (typeOf b)

-- | branch: B [T] [F] -> execute T if true, F if false
opBranch :: VM ()
opBranch = do
  f <- pop "branch"
  t <- pop "branch"
  b <- pop "branch"
  case (b, t, f) of
    (JBool True, JQuote tBody, _) -> evalJoyList tBody
    (JBool False, _, JQuote fBody) -> evalJoyList fBody
    (JBool _, _, _) -> throwError $ TypeError "branch" "quotations" "non-quotation"
    _ -> throwError $ TypeError "branch" "boolean" (typeOf b)

-----------------------------------------------------------------------------
-- List Processing Combinators
-----------------------------------------------------------------------------

-- | map: [X Y Z] [P] -> [P(X) P(Y) P(Z)]
opMap :: VM ()
opMap = do
  q <- pop "map"
  xs <- pop "map"
  case (xs, q) of
    (JQuote items, JQuote body) -> do
      results <- mapM (applyQuote body) items
      push $ JQuote results
    _ -> throwError $ TypeError "map" "two quotations" (typeOf xs <> " and " <> typeOf q)
  where
    applyQuote body item = do
      savedStack <- gets vmStack
      push item
      evalJoyList body
      result <- pop "map"
      modify $ \s -> s { vmStack = savedStack }
      return result

-- | filter: [X Y Z] [P] -> elements where P is true
opFilter :: VM ()
opFilter = do
  q <- pop "filter"
  xs <- pop "filter"
  case (xs, q) of
    (JQuote items, JQuote body) -> do
      results <- filterM (testQuote body) items
      push $ JQuote results
    _ -> throwError $ TypeError "filter" "two quotations" (typeOf xs <> " and " <> typeOf q)
  where
    testQuote body item = do
      savedStack <- gets vmStack
      push item
      evalJoyList body
      result <- pop "filter"
      modify $ \s -> s { vmStack = savedStack }
      case result of
        JBool b -> return b
        _ -> throwError $ TypeError "filter" "boolean" (typeOf result)

-- | fold: [X Y Z] Init [P] -> result of folding P over list
opFold :: VM ()
opFold = do
  q <- pop "fold"
  initVal <- pop "fold"
  xs <- pop "fold"
  case (xs, q) of
    (JQuote items, JQuote body) -> do
      savedStack <- gets vmStack
      result <- foldM (foldStep body) initVal items
      modify $ \s -> s { vmStack = savedStack }
      push result
    _ -> throwError $ TypeError "fold" "quotation, value, quotation" (typeOf xs <> " and " <> typeOf q)
  where
    foldStep body acc item = do
      push acc >> push item
      evalJoyList body
      pop "fold"

-- | step: [X Y Z] [P] -> execute P for each element
opStep :: VM ()
opStep = do
  q <- pop "step"
  xs <- pop "step"
  case (xs, q) of
    (JQuote items, JQuote body) -> mapM_ (\item -> push item >> evalJoyList body) items
    _ -> throwError $ TypeError "step" "two quotations" (typeOf xs <> " and " <> typeOf q)

-- | split: [X Y Z] [P] -> [[pass] [fail]]
opSplit :: VM ()
opSplit = do
  q <- pop "split"
  xs <- pop "split"
  case (xs, q) of
    (JQuote items, JQuote body) -> do
      (pass, fail') <- partitionM (testQuote body) items
      push $ JQuote [JQuote pass, JQuote fail']
    _ -> throwError $ TypeError "split" "two quotations" (typeOf xs <> " and " <> typeOf q)
  where
    testQuote body item = do
      savedStack <- gets vmStack
      push item
      evalJoyList body
      result <- pop "split"
      modify $ \s -> s { vmStack = savedStack }
      case result of
        JBool b -> return b
        _ -> throwError $ TypeError "split" "boolean" (typeOf result)
    partitionM _ [] = return ([], [])
    partitionM p (x:xs') = do
      b <- p x
      (pass, fail') <- partitionM p xs'
      return $ if b then (x:pass, fail') else (pass, x:fail')

-- | times: N [P] -> execute P N times
opTimes :: VM ()
opTimes = do
  q <- pop "times"
  n <- pop "times"
  case (n, q) of
    (JInt count, JQuote body) ->
      replicateM_ (fromIntegral count) (evalJoyList body)
    _ -> throwError $ TypeError "times" "integer and quotation" (typeOf n <> " and " <> typeOf q)

-----------------------------------------------------------------------------
-- Recursion Combinators
-----------------------------------------------------------------------------

-- | linrec: [If] [Then] [Rec1] [Rec2] -> linear recursion
-- If condition true, do Then. Otherwise: Rec1, recurse, Rec2
opLinrec :: VM ()
opLinrec = do
  rec2 <- pop "linrec"
  rec1 <- pop "linrec"
  then' <- pop "linrec"
  if' <- pop "linrec"
  case (if', then', rec1, rec2) of
    (JQuote ifBody, JQuote thenBody, JQuote rec1Body, JQuote rec2Body) ->
      linrecLoop ifBody thenBody rec1Body rec2Body
    _ -> throwError $ TypeError "linrec" "four quotations" "non-quotation"
  where
    linrecLoop ifBody thenBody rec1Body rec2Body = do
      savedStack <- gets vmStack
      evalJoyList ifBody
      cond <- pop "linrec"
      modify $ \s -> s { vmStack = savedStack }
      case cond of
        JBool True -> evalJoyList thenBody
        JBool False -> do
          evalJoyList rec1Body
          linrecLoop ifBody thenBody rec1Body rec2Body
          evalJoyList rec2Body
        _ -> throwError $ TypeError "linrec" "boolean" (typeOf cond)

-- | primrec: [Value] [Combiner] -> primitive recursion
opPrimrec :: VM ()
opPrimrec = do
  combiner <- pop "primrec"
  val <- pop "primrec"
  n <- pop "primrec"
  case (val, combiner) of
    (JQuote valBody, JQuote combinerBody) -> do
      case n of
        JInt 0 -> evalJoyList valBody
        JInt i -> do
          push (JInt (i - 1))
          push (JQuote valBody)
          push (JQuote combinerBody)
          opPrimrec
          push (JInt i)
          evalJoyList combinerBody
        JQuote [] -> evalJoyList valBody
        JQuote (x:xs) -> do
          push (JQuote xs)
          push (JQuote valBody)
          push (JQuote combinerBody)
          opPrimrec
          push x
          evalJoyList combinerBody
        _ -> throwError $ TypeError "primrec" "integer or list" (typeOf n)
    _ -> throwError $ TypeError "primrec" "two quotations" "non-quotation"

-- | tailrec: [If] [Then] [Rec] -> tail recursive loop
opTailrec :: VM ()
opTailrec = do
  rec' <- pop "tailrec"
  then' <- pop "tailrec"
  if' <- pop "tailrec"
  case (if', then', rec') of
    (JQuote ifBody, JQuote thenBody, JQuote recBody) ->
      tailrecLoop ifBody thenBody recBody
    _ -> throwError $ TypeError "tailrec" "three quotations" "non-quotation"
  where
    tailrecLoop ifBody thenBody recBody = do
      savedStack <- gets vmStack
      evalJoyList ifBody
      cond <- pop "tailrec"
      modify $ \s -> s { vmStack = savedStack }
      case cond of
        JBool True -> evalJoyList thenBody
        JBool False -> do
          evalJoyList recBody
          tailrecLoop ifBody thenBody recBody
        _ -> throwError $ TypeError "tailrec" "boolean" (typeOf cond)

-- | genrec: [If] [Then] [Rec1] [Rec2] -> general recursion
opGenrec :: VM ()
opGenrec = do
  rec2 <- pop "genrec"
  rec1 <- pop "genrec"
  then' <- pop "genrec"
  if' <- pop "genrec"
  case (if', then', rec1, rec2) of
    (JQuote ifBody, JQuote thenBody, JQuote rec1Body, JQuote rec2Body) -> do
      savedStack <- gets vmStack
      evalJoyList ifBody
      cond <- pop "genrec"
      modify $ \s -> s { vmStack = savedStack }
      case cond of
        JBool True -> evalJoyList thenBody
        JBool False -> do
          evalJoyList rec1Body
          -- Push the genrec call as a quotation for Rec2 to use
          push $ JQuote (ifBody ++ [JWord "genrec-continue"])
          evalJoyList rec2Body
        _ -> throwError $ TypeError "genrec" "boolean" (typeOf cond)
    _ -> throwError $ TypeError "genrec" "four quotations" "non-quotation"

-- | binrec: [If] [Then] [Rec1] [Rec2] -> binary recursion (like quicksort)
opBinrec :: VM ()
opBinrec = do
  rec2 <- pop "binrec"
  rec1 <- pop "binrec"
  then' <- pop "binrec"
  if' <- pop "binrec"
  case (if', then', rec1, rec2) of
    (JQuote ifBody, JQuote thenBody, JQuote rec1Body, JQuote rec2Body) ->
      binrecLoop ifBody thenBody rec1Body rec2Body
    _ -> throwError $ TypeError "binrec" "four quotations" "non-quotation"
  where
    binrecLoop ifBody thenBody rec1Body rec2Body = do
      savedStack <- gets vmStack
      evalJoyList ifBody
      cond <- pop "binrec"
      modify $ \s -> s { vmStack = savedStack }
      case cond of
        JBool True -> evalJoyList thenBody
        JBool False -> do
          evalJoyList rec1Body  -- Split into two parts
          -- Recurse on second part
          binrecLoop ifBody thenBody rec1Body rec2Body
          opSwap
          -- Recurse on first part
          binrecLoop ifBody thenBody rec1Body rec2Body
          opSwap
          -- Combine results
          evalJoyList rec2Body
        _ -> throwError $ TypeError "binrec" "boolean" (typeOf cond)

-----------------------------------------------------------------------------
-- Type Predicates
-----------------------------------------------------------------------------

opIsInteger, opIsFloat, opIsNumber, opIsChar, opIsString, opIsList, opIsLeaf, opIsLogical :: VM ()

opIsInteger = do
  a <- pop "integer?"
  push a
  push $ JBool $ case a of JInt _ -> True; _ -> False

opIsFloat = do
  a <- pop "float?"
  push a
  push $ JBool $ case a of JFloat _ -> True; _ -> False

opIsNumber = do
  a <- pop "number?"
  push a
  push $ JBool $ case a of JInt _ -> True; JFloat _ -> True; _ -> False

opIsChar = do
  a <- pop "char?"
  push a
  push $ JBool $ case a of JChar _ -> True; _ -> False

opIsString = do
  a <- pop "string?"
  push a
  push $ JBool $ case a of JString _ -> True; _ -> False

opIsList = do
  a <- pop "list?"
  push a
  push $ JBool $ case a of JQuote _ -> True; _ -> False

opIsLeaf = do
  a <- pop "leaf?"
  push a
  push $ JBool $ case a of JQuote _ -> False; _ -> True

opIsLogical = do
  a <- pop "logical?"
  push a
  push $ JBool $ case a of JBool _ -> True; _ -> False

-----------------------------------------------------------------------------
-- Type Conversions
-----------------------------------------------------------------------------

opOrd, opChr, opStrtol :: VM ()

opOrd = do
  a <- pop "ord"
  case a of
    JChar c -> push $ JInt (fromIntegral $ fromEnum c)
    _ -> throwError $ TypeError "ord" "char" (typeOf a)

opChr = do
  a <- pop "chr"
  case a of
    JInt n -> push $ JChar (toEnum $ fromIntegral n)
    _ -> throwError $ TypeError "chr" "integer" (typeOf a)

opStrtol = do
  a <- pop "strtol"
  case a of
    JString s -> case reads (T.unpack s) of
      [(n, "")] -> push $ JInt n
      _ -> throwError $ RuntimeError "strtol: invalid integer string"
    _ -> throwError $ TypeError "strtol" "string" (typeOf a)

-----------------------------------------------------------------------------
-- I/O Operations
-----------------------------------------------------------------------------

-- Note: These are simplified for pure evaluation
-- In a real implementation, they would need IO integration

opPut, opPutch, opPrint :: VM ()

opPut = do
  a <- pop "put"
  -- In real Joy, this would output to stdout
  -- For now, we just consume the value
  return ()

opPutch = do
  a <- pop "putch"
  case a of
    JChar _ -> return ()  -- Would output char
    JInt n  -> return ()  -- Would output char with code n
    _ -> throwError $ TypeError "putch" "char or integer" (typeOf a)

opPrint = do
  st <- gets vmStack
  -- In real Joy, this prints the stack
  return ()

-----------------------------------------------------------------------------
-- Miscellaneous
-----------------------------------------------------------------------------

-- | unit: X -> [X]
opUnit :: VM ()
opUnit = do
  x <- pop "unit"
  push $ JQuote [x]

-- | pair: X Y -> [X Y]
opPair :: VM ()
opPair = do
  y <- pop "pair"
  x <- pop "pair"
  push $ JQuote [x, y]

-- | unpair: [X Y] -> X Y
opUnpair :: VM ()
opUnpair = do
  q <- pop "unpair"
  case q of
    JQuote [x, y] -> push x >> push y
    JQuote _ -> throwError $ RuntimeError "unpair: quotation must have exactly 2 elements"
    _ -> throwError $ TypeError "unpair" "quotation" (typeOf q)

-- | infra: [X Y Z] [P] -> [result of P on [X Y Z]]
opInfra :: VM ()
opInfra = do
  q <- pop "infra"
  xs <- pop "infra"
  case (xs, q) of
    (JQuote stk, JQuote body) -> do
      savedStack <- gets vmStack
      modify $ \s -> s { vmStack = stk }
      evalJoyList body
      newStack <- gets vmStack
      modify $ \s -> s { vmStack = savedStack }
      push $ JQuote newStack
    _ -> throwError $ TypeError "infra" "two quotations" (typeOf xs <> " and " <> typeOf q)

-- | cleave: X [P] [Q] -> P(X) Q(X)
opCleave :: VM ()
opCleave = do
  q2 <- pop "cleave"
  q1 <- pop "cleave"
  x <- pop "cleave"
  case (q1, q2) of
    (JQuote body1, JQuote body2) -> do
      -- Apply first quotation
      push x
      evalJoyList body1
      r1 <- pop "cleave"
      -- Apply second quotation
      push x
      evalJoyList body2
      r2 <- pop "cleave"
      -- Push both results
      push r1 >> push r2
    _ -> throwError $ TypeError "cleave" "two quotations" (typeOf q1 <> " and " <> typeOf q2)

-- | define: [body] name -> (adds definition to environment)
opDefine :: VM ()
opDefine = do
  name <- pop "define"
  body <- pop "define"
  case (name, body) of
    (JWord n, JQuote b) -> define n b
    (JString n, JQuote b) -> define n b
    _ -> throwError $ TypeError "define" "word and quotation" (typeOf name <> " and " <> typeOf body)

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------

-- | Get type name for error messages
typeOf :: Joy -> Text
typeOf (JInt _)    = "integer"
typeOf (JFloat _)  = "float"
typeOf (JBool _)   = "boolean"
typeOf (JChar _)   = "char"
typeOf (JString _) = "string"
typeOf (JWord _)   = "word"
typeOf (JQuote _)  = "quotation"

-----------------------------------------------------------------------------
-- Running Programs
-----------------------------------------------------------------------------

-- | Run a program with empty initial state
runProgram :: [Joy] -> Either VMError Stack
runProgram prog = runProgramWithEnv M.empty prog

-- | Run a program with an initial environment
runProgramWithEnv :: Env -> [Joy] -> Either VMError Stack
runProgramWithEnv env prog = vmStack <$> runProgramStateWithEnv env prog

-- | Run a program with an initial environment and return the full VM state.
runProgramStateWithEnv :: Env -> [Joy] -> Either VMError VMState
runProgramStateWithEnv env prog =
  case runExcept $ execStateT (evalJoyList prog) (VMState [] env) of
    Left err -> Left err
    Right st -> Right st
