module Test.CoreFn.Names
  ( testNames
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Names (ModuleName(..), OpName(..), ProperName(..))
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)
import Test.Util (assertEqual, expectRight)

testNames :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testNames = do
  log ""
  log "Test Names"

  testModuleName
  testOpName
  testProperName

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

  -- |
  -- OpName
  --
  testOpName = do
    let description = "OpName from JSON results in success"

    let json = """
      "Control.Bind.bind"
    """

    let result = readJSON json :: Either ForeignError OpName

    expectRight description result \(x) ->
      assertEqual x (OpName "Control.Bind.bind")

  -- |
  -- ProperName
  --
  testProperName = do
    let description = "ProperName from JSON results in success"

    let json = """
      "Nothing"
    """

    let result = readJSON json :: Either ForeignError ProperName

    expectRight description result \(x) ->
      assertEqual x (ProperName "Nothing")
