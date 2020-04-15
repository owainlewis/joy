{-# LANGUAGE OverloadedStrings #-}
module Language.Joy where

import qualified Control.Arrow       as Arrow
import           Language.Joy.AST
import           Language.Joy.Parser

-- TODO I'm just dumping / losing all the useful context here but will come back later and fix
runJoy :: String -> Either String [Joy]
runJoy s = Arrow.left show $ readExpr s
