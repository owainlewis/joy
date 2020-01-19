{-# LANGUAGE OverloadedStrings #-}
module Language.Joy
  (Program, run, Joy(..)) where

import           Language.Joy.Core
import qualified Language.Joy.Parser as P
import           Language.Joy.VM

eval e =
  let ast = P.readExpr e in
    case ast of
      Right exprs -> Just exprs
      Left e      -> Nothing
