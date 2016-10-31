module Test.CoreFn.Module
  ( testModule
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..))
import Data.Either (Either)
import Data.Foreign (ForeignError(..))
import Data.Foreign.Class (readJSON)
import Test.Util (assertEqual, expectLeft, expectRight)

testModule :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testModule = do
  log ""
  log "Test Module"

  testMissingName
  testName
  testExports
  testImports

  where

  -- |
  -- Name
  --
  testMissingName = do
    let description = "Missing module name in JSON results in error"

    let json = """
      {}
    """

    let result = readJSON json :: Either ForeignError Module

    expectLeft description result \(x) ->
      assertEqual x (JSONError "Module name not found")

  testName = do
    let description = "Module name from JSON results in success"

    let json = """
      {
        "Main": {
          "imports": [],
          "exports": []
        }
      }
    """

    let result = readJSON json :: Either ForeignError Module

    expectRight description result \(Module x) ->
      assertEqual x.moduleName (ModuleName "Main")

  -- |
  -- Exports
  --
  testExports = do
    let description = "Module exports from JSON result in success"

    let json = """
      {
        "Main": {
          "imports": [],
          "exports": [
            "main"
          ]
        }
      }
    """

    let result = readJSON json :: Either ForeignError Module

    expectRight description result \(Module x) ->
      assertEqual x.moduleExports
        [ (Ident "main")
        ]

  -- |
  -- Imports
  --
  testImports = do
    let description = "Module imports from JSON result in success"

    let json = """
      {
        "Main": {
          "imports": [
            "Prim"
          ],
          "exports": []
        }
      }
    """

    let result = readJSON json :: Either ForeignError Module

    expectRight description result \(Module x) ->
      assertEqual x.moduleImports
        [ (ModuleName "Prim")
        ]
