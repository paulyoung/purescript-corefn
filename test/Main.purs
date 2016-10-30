module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.CoreFn.FromJSON (testFromJSON)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
main = do
  testFromJSON
  log ""
