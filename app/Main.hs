module Main where

import Language.Joy

program :: Program
program = [ JInt 10
          , JWord "DUP"
          ]

main = run program
