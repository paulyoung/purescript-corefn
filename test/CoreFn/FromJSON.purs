module Test.CoreFn.FromJSON
  ( testFromJSON
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..))
import CoreFn.ModuleName (ModuleName(..))
import Data.Foreign (ForeignError(..))
import Test.Util (assertEqual, expectLeft, expectRight)

-- TODO: use purescript-test-unit when compatible with 0.10
testFromJSON :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testFromJSON = do

  -- |
  -- Name
  --
  expectLeft (moduleFromJSON """
    {}
  """) \(x) ->
    assertEqual x (JSONError "Module name not found")

  expectRight (moduleFromJSON """
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
  expectRight (moduleFromJSON """
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
  expectRight (moduleFromJSON """
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
