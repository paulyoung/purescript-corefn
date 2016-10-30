module Test.CoreFn.FromJSON
  ( testFromJSON
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Test.CoreFn.FromJSON.Module (testModuleFromJSON)

-- TODO: use purescript-test-unit when compatible with 0.10
testFromJSON :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testFromJSON = do
  testModuleFromJSON
