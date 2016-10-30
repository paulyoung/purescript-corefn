module Test.CoreFn.FromJSON.Module
  ( testModuleFromJSON
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.Names (ModuleName(..))
import Data.Foreign (ForeignError(..))
import Test.Util (assertEqual, expectLeft, expectRight)

testModuleFromJSON :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testModuleFromJSON = do
  log ""
  log "Test.CoreFn.FromJSON.Module"

  -- |
  -- Name
  --
  expectLeft "Missing module name in JSON results in error" (moduleFromJSON """
    {}
  """) \(x) ->
    assertEqual x (JSONError "Module name not found")

  expectRight "Module name from JSON results in success" (moduleFromJSON """
    {
      "Main": {
        "imports": [],
        "exports": []
      }
    }
  """) \(Module x) ->
    assertEqual x.moduleName (ModuleName "Main")

  -- |
  -- Exports
  --
  expectRight "Module exports from JSON result in success" (moduleFromJSON """
    {
      "Main": {
        "imports": [],
        "exports": [
          "main"
        ]
      }
    }
  """) \(Module x) ->
    assertEqual x.moduleExports
      [ (Ident "main")
      ]

  -- |
  -- Imports
  --
  expectRight "Module imports from JSON result in success" (moduleFromJSON """
    {
      "Main": {
        "imports": [
          "Prim"
        ],
        "exports": []
      }
    }
  """) \(Module x) ->
    assertEqual x.moduleImports
      [ (ModuleName "Prim")
      ]
