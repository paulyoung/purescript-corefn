module Test.CoreFn.Module
  ( testModule
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT)
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..))
import Data.Foreign (ForeignError(..))
import Data.Foreign.Class (readJSON)
import Data.Identity (Identity)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity Module

    expectLeft description (runExcept result) \(x) ->
      assertEqual x (singleton (JSONError "Module name not found"))

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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity Module

    expectRight description (runExcept result) \(Module x) ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity Module

    expectRight description (runExcept result) \(Module x) ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity Module

    expectRight description (runExcept result) \(Module x) ->
      assertEqual x.moduleImports
        [ (ModuleName "Prim")
        ]
