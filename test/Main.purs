module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.CoreFn.Expr (testExpr, testLiterals)
import Test.CoreFn.Ident (testIdent)
import Test.CoreFn.Module (testModule)
import Test.CoreFn.Names (testNames)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
main = do
  testIdent
  testNames
  testLiterals
  testExpr
  testModule
  log ""
