module Test.CoreFn.Module
  ( testModule
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Expr (Bind(..), Expr(..), Literal(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Module (Module(..), readModuleJSON)
import CoreFn.Names (ModuleName(..), Qualified(..))
import Data.Foreign (ForeignError(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Test.Util (assertEqual, expectFailure, expectSuccess)

testModule :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testModule = do
  log ""
  log "Test Module"

  testMissingName
  testName
  testImports
  testBuiltWith
  testExports
  testDecls
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
          "builtWith": "0.10.1",
          "exports": [],
          "decls": [],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleName (ModuleName "Main")

  -- |
  -- Built with version
  --
  testBuiltWith = do
    let description = "Built with version from JSON result in success"

    let json = """
      {
        "Main": {
          "imports": [
            "Prim"
          ],
          "builtWith": "0.10.1",
          "exports": [],
          "decls": [],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.builtWith "0.10.1"

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
          "builtWith": "0.10.1",
          "exports": [],
          "decls": [],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleImports
        [ ModuleName "Prim"
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
          "builtWith": "0.10.1",
          "exports": [
            "main"
          ],
          "decls": [],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleExports
        [ Ident "main"
        ]

  -- |
  -- Declarations
  --
  testDecls = do
    let description = "Module declarations from JSON result in success"

    let json = """
      {
        "Main": {
          "imports": [],
          "builtWith": "0.10.1",
          "exports": [
            "main"
          ],
          "decls": [
            {
              "main": [
                "App",
                [
                  "Var",
                  "Control.Monad.Eff.Console.log"
                ],
                [
                  "Literal",
                  [
                    "StringLiteral",
                    "Hello world!"
                  ]
                ]
              ]
            }
          ],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) -> do
      let ident = Ident "main"
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      let var = Var unit qualified
      let literal = Literal unit (StringLiteral "Hello world!")
      let expr = App unit var literal
      let decl = NonRec unit ident expr
      assertEqual x.moduleDecls [ decl ]

  -- |
  -- Foreign declarations
  --
  testForeign = do
    let description = "Foreign declarations from JSON result in success"

    let json = """
      {
        "Main": {
          "imports": [],
          "builtWith": "0.10.1",
          "exports": [],
          "decls": [],
          "foreign": [
            "log"
          ]
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) ->
      assertEqual x.moduleForeign
        [ Ident "log"
        ]
