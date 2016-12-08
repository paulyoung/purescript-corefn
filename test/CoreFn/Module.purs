module Test.CoreFn.Module
  ( testModule
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..), readModuleJSON)
import CoreFn.Names (ModuleName(..))
import Data.Foreign (ForeignError(..))
import Data.List.NonEmpty (singleton)
import Test.Util (assertEqual, expectFailure, expectSuccess)

testModule :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testModule = do
  log ""
  log "Test Module"

  testMissingName
  testName
  testImports
  testExports
  testForeign

  where

  -- |
  -- Name
  --
  testMissingName = do
    let description = "Missing module name in JSON results in error"

    let json = """
      {}
    """

    expectFailure description (readModuleJSON json) \x ->
      assertEqual x (singleton (ForeignError "Module name not found"))

  testName = do
    let description = "Module name from JSON results in success"

    let json = """
      {
        "Main": {
          "imports": [],
          "exports": [],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleName (ModuleName "Main")

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
          "exports": [],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleImports
        [ (ModuleName "Prim")
        ]

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
          ],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleExports
        [ (Ident "main")
        ]

  -- |
  -- Foreign declarations
  --
  testForeign = do
    let description = "Foreign declarations from JSON result in success"

    let json = """
      {
        "Main": {
          "imports": [],
          "exports": [],
          "foreign": [
            "log"
          ]
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleForeign
        [ (Ident "log")
        ]
