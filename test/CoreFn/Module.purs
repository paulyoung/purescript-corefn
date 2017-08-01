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
import CoreFn.Names (ModuleName(..))
import Data.Foreign (ForeignError(..))
import Data.List.NonEmpty (singleton)
import Data.Tuple (Tuple(..))
import Test.Util (assertEqual, expectFailure, expectSuccess)

testModule :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
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
              "x": [
                "Literal",
                [
                  "StringLiteral",
                  "x"
                ]
              ]
            },
            {
              "y": [
                "Literal",
                [
                  "StringLiteral",
                  "y"
                ]
              ]
            }
          ],
          "foreign": []
        }
      }
    """

    expectSuccess description (readModuleJSON json) \(Module x) -> do
      let xIdent = Ident "x"
      let xLiteral = Literal unit (StringLiteral "x")
      let xBinding = Tuple (Tuple unit xIdent) xLiteral
      let xDecl = Bind [ xBinding ]

      let yIdent = Ident "y"
      let yLiteral = Literal unit (StringLiteral "y")
      let yBinding = Tuple (Tuple unit yIdent) yLiteral
      let yDecl = Bind [ yBinding ]

      assertEqual x.moduleDecls [ xDecl, yDecl ]

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
