module Test.CoreFn.FromJSON
  ( testFromJSON
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throwException, error)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Module (Module(..))
import CoreFn.ModuleName (ModuleName(..))
import Data.Either (either)

-- TODO: use purescript-test-unit when compatible with 0.10
testFromJSON :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testFromJSON = do

  -- Expect Left when no module name is missing
  test (moduleFromJSON "{}") logSuccessShow expectLeft

  -- Expect Right when module name is present
  -- Throw an exception if the name doesn't match
  test (moduleFromJSON "{ \"Main\": null }") expectRight \(Module x) ->
    if x.moduleName == ModuleName "Main"
    then logSuccessShow x.moduleName
    else failure "expected module name to be \"Main\""

  where

  test x y z = either y z x

  expectLeft _ = failure "expected Left"
  expectRight _ = failure "expected Right"

  green = "\x1b[32m"
  reset = "\x1b[0m"
  check = "✓"
  greenCheck = green <> check <> reset

  logSuccess x = log $ "  " <> greenCheck <> " " <> x

  logSuccessShow :: forall a. (Show a) => a -> Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
  logSuccessShow x = logSuccess $ show x

  failure = throwException <<< error
