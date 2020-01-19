{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Language.Joy.Core
    ( Joy(..)
    , Program
    , ProgramError(..)
    , dup')
where

data Instr =
    DUP
  | SWAP
    deriving (Eq, Ord, Show)

data Joy =
    JWord String
  | JString String
  | JInt Integer
  | JFloat Double
  | JBool Bool
  | JChar Char
  | JQuote [Joy]
  | JInstr Instr
    deriving (Eq, Ord, Show)

type Program = [Joy]

-- A simple function takes the current stack and modifies it
-- returning either a well defined error or the modified stack
type SimpleFunction = [Joy] -> Either ProgramError [Joy]

data ProgramError =
    ArityError Int Int -- (expected, actual)
    deriving (Eq, Ord)

-- | Stack manipulation logic. We implement the following virtual machine operations
--
-- EvA( dup   , [X   | S])  =  [X X | S]
-- EvA( swap  , [X Y | S])  =  [Y X | S]
-- EvA( pop   , [X   | S])  =         S
-- EvA( stack ,        S )  =  [S   | S]
-- EvA(unstack, [L    | S])  =  L         (L is a quotation of list)
--

withN :: Foldable t => Int -> (t a -> b) -> t a -> Either ProgramError b
withN n f s =
  let l = length s in
  if  l < n then Left $ ArityError n l
            else Right (f s)

-- DEFINE dup as [X | S]  =>  [X X | S]
dup' :: [Joy] -> Either ProgramError [Joy]
dup' s = withN 1 f s where f (x:xs) = x:x:xs
