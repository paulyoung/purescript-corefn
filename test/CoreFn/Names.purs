module Test.CoreFn.Names
  ( testNames
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Names (ModuleName(..))
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)
import Test.Util (assertEqual, expectRight)

testNames :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testNames = do
  log ""
  log "Test Names"

  testModuleName

  where

  -- |
  -- ModuleName
  --
  testModuleName = do
    let description = "ModuleName from JSON results in success"

    let json = """
      "Main"
    """

    let result = readJSON json :: Either ForeignError ModuleName

    expectRight description result \(x) ->
      assertEqual x (ModuleName "Main")
