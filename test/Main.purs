module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Node.FS (FS)
import Test.CoreFn.Expr (testBinders, testBindings, testCaseAlternatives, testExpr, testLiterals)
import Test.CoreFn.Ident (testIdent)
import Test.CoreFn.Module (testModule)
import Test.CoreFn.Names (testNames)
import Test.Parsing (testParsing)

main :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | e) Unit
main = do
  testIdent
  testNames
  testLiterals
  testExpr
  testBindings
  testCaseAlternatives
  testBinders
  testModule
  testParsing
  log ""
