module Test.CoreFn.Expr
  ( testBinders
  , testBindings
  , testCaseAlternatives
  , testExpr
  , testLiterals
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Expr (Bind(..), Binder(..), CaseAlternative(..), Expr(..), Literal(..), readBindJSON, readBinderJSON, readCaseAlternativeJSON, readExprJSON, readLiteralJSON)
import CoreFn.Ident (Ident(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Util (assertEqual, expectFailure, expectSuccess)

testLiterals :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
testLiterals = do
  log ""
  log "Test Literals"

  testNumericIntLiteral
  testNumericNumberLiteral
  testStringLiteral
  testCharLiteral
  testBooleanLiteral
  testArrayLiteral
  testObjectLiteral
  testUnknownLiteral

  where

  -- |
  -- NumericLiteral (Int)
  --
  testNumericIntLiteral = do
    let description = "NumericLiteral (Int) from JSON results in success"

    let json = """
      [
        "IntLiteral",
        42
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (NumericLiteral (Left 42))

  -- |
  -- NumericLiteral (Number)
  --
  testNumericNumberLiteral = do
    let description = "NumericLiteral (Number) from JSON results in success"

    let json = """
      [
        "NumberLiteral",
        3.14
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (NumericLiteral (Right 3.14))

  -- |
  -- StringLiteral
  --
  testStringLiteral = do
    let description = "StringLiteral from JSON results in success"

    let json = """
      [
        "StringLiteral",
        "Hello World!"
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (StringLiteral "Hello World!")

  -- |
  -- CharLiteral
  --
  testCharLiteral = do
    let description = "CharLiteral from JSON results in success"

    let json = """
      [
        "CharLiteral",
        "a"
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (CharLiteral 'a')

  -- |
  -- BooleanLiteral
  --
  testBooleanLiteral = do
    let description = "BooleanLiteral from JSON results in success"

    let json = """
      [
        "BooleanLiteral",
        true
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x (BooleanLiteral true)

  -- |
  -- ArrayLiteral
  --
  testArrayLiteral = do
    let description = "ArrayLiteral from JSON results in success"

    let json = """
      [
        "ArrayLiteral",
        [
          [
            "Literal",
            [
              "StringLiteral",
              "Hello world!"
            ]
          ]
        ]
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x $ ArrayLiteral
        [ Literal unit (StringLiteral "Hello world!")
        ]

  -- |
  -- ObjectLiteral
  --
  testObjectLiteral = do
    let description = "ObjectLiteral from JSON results in success"

    let json = """
      [
        "ObjectLiteral",
        {
          "hello": [
            "Literal",
            [
              "StringLiteral",
              "world!"
            ]
          ]
        }
      ]
    """

    expectSuccess description (readLiteralJSON json) \x ->
      assertEqual x $ ObjectLiteral
        [ Tuple "hello" (Literal unit (StringLiteral "world!"))
        ]

  -- |
  -- Unknown
  --
  testUnknownLiteral = do
    let description = "Unknown literal from JSON results in error"

    let json = """
      [
        "SomeLiteral",
        "some value"
      ]
    """

    expectFailure description (readLiteralJSON json) \x ->
      assertEqual x (singleton (ForeignError "Unknown literal: SomeLiteral"))

testExpr :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
testExpr = do
  log ""
  log "Test Expr"

  testLiteralExpr
  testConstructorExpr
  testAccessorExpr
  testObjectUpdateExpr
  testAbsExpr
  testAppExpr
  testVarExpr
  testCaseExpr
  testLetExpr
  testUnknownExpr

  where

  -- |
  -- Literal
  --
  testLiteralExpr = do
    let description = "Literal from JSON results in success"

    let json = """
      [
        "Literal",
        [
          "StringLiteral",
          "Hello world!"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x ->
      assertEqual x (Literal unit (StringLiteral "Hello world!"))

  -- |
  -- Constructor
  --
  testConstructorExpr = do
    let description = "Constructor from JSON results in success"

    let json = """

      [
        "Constructor",
        "Either",
        "Left",
        [
          "x"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let type' = ProperName "Either"
      let name = ProperName "Left"
      let fields = [Ident "x"]
      assertEqual x (Constructor unit type' name fields)

  -- |
  -- Accessor
  --
  testAccessorExpr = do
    let description = "Accessor from JSON results in success"

    let json = """
      [
        "Accessor",
        "value",
        [
          "Var",
          "x"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let string = "value"
      let qualified = Qualified Nothing (Ident "x")
      let var = Var unit qualified
      assertEqual x (Accessor unit string var)


  -- |
  -- ObjectUpdate
  --
  testObjectUpdateExpr = do
    let description = "ObjectUpdate from JSON results in success"

    let json = """
      [
        "ObjectUpdate",
        [
          "Var",
          "value"
        ],
        {
          "x": [
            "Literal",
            [
              "StringLiteral",
              "y"
            ]
          ]
        }
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let qualified = Qualified Nothing (Ident "value")
      let record = Var unit qualified
      let updates = [Tuple "x" (Literal unit (StringLiteral "y"))]
      assertEqual x (ObjectUpdate unit record updates)

  -- |
  -- Abs
  --
  testAbsExpr = do
    let description = "Abs from JSON results in success"

    let json = """
      [
        "Abs",
        "x",
        [
          "Var",
          "x"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let ident = Ident "x"
      let qualified = Qualified Nothing (Ident "x")
      let var = Var unit qualified
      assertEqual x (Abs unit ident var)

  -- |
  -- App
  --
  testAppExpr = do
    let description = "App from JSON results in success"

    let json = """
      [
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
    """

    expectSuccess description (readExprJSON json) \x -> do
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      let var = Var unit qualified
      let literal = Literal unit (StringLiteral "Hello world!")
      assertEqual x (App unit var literal)

  -- |
  -- Var
  --
  testVarExpr = do
    let description = "Var from JSON results in success"

    let json = """
      [
        "Var",
        "Control.Monad.Eff.Console.log"
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      assertEqual x (Var unit qualified)

  -- |
  -- Case
  --
  testCaseExpr = do
    let description = "Case from JSON results in success"

    let json = """
      [
        "Case",
        [
          [
            "Var",
            "x"
          ]
        ],
        [
          [
            [
              [
                "ConstructorBinder",
                "Data.Identity.Identity",
                "Data.Identity.Identity",
                [
                  [
                    "VarBinder",
                    "a"
                  ]
                ]
              ]
            ],
            [
              "Var",
              "a"
            ]
          ]
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let var = Var unit (Qualified Nothing (Ident "x"))
      let moduleName = Just (ModuleName "Data.Identity")
      let type' = Qualified moduleName (ProperName "Identity")
      let constructor = Qualified moduleName (ProperName "Identity")
      let binder = VarBinder unit (Ident "a")
      let binders = [ConstructorBinder unit type' constructor [binder]]
      let result = Right (Var unit (Qualified Nothing (Ident "a")))
      let alternative = CaseAlternative { binders, result }
      assertEqual x (Case unit [var] [alternative])

  -- |
  -- Let
  --
  testLetExpr = do
    let description = "Let from JSON results in success"

    let json = """
      [
        "Let",
        [
          {
            "x": [
              "Literal",
              [
                "StringLiteral",
                "y"
              ]
            ]
          }
        ],
        [
          "Var",
          "x"
        ]
      ]
    """

    expectSuccess description (readExprJSON json) \x -> do
      let expr = Literal unit (StringLiteral "y")
      let binding = Bind [Tuple (Tuple unit (Ident "x")) expr]
      let var = Var unit (Qualified Nothing (Ident "x"))
      assertEqual x (Let unit [binding] var)

  -- |
  -- Unknown
  --
  testUnknownExpr = do
    let description = "Unknown expression from JSON results in error"

    let json = """
      [
        "SomeExpression",
        [
          "Literal",
          [
            "StringLiteral",
            "Hello world!"
          ]
        ]
      ]
    """

    expectFailure description (readExprJSON json) \x ->
      assertEqual x (singleton (ForeignError "Unknown expression: SomeExpression"))

testBindings :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
testBindings = do
  log ""
  log "Test Bind"

  testNoBindings
  testNonRecBind
  testRecBind

  where

  testNoBindings = do
    let description = "No bindings from JSON results in success"

    let json = """
      {}
    """

    expectSuccess description (readBindJSON json) \x ->
      assertEqual x (Bind [])

  -- |
  -- Non-recursive binding
  --
  testNonRecBind = do
    let description = "Non-recursive binding from JSON result in success"

    let json = """
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
    """

    expectSuccess description (readBindJSON json) \x -> do
      let ident = Ident "main"
      let moduleName = Just (ModuleName "Control.Monad.Eff.Console")
      let qualified = Qualified moduleName (Ident "log")
      let var = Var unit qualified
      let literal = Literal unit (StringLiteral "Hello world!")
      let app = App unit var literal
      let binding = Tuple (Tuple unit ident) app
      assertEqual x (Bind [binding])

  -- |
  -- Mutually recursive bindings
  --
  testRecBind = do
    let description = "Mutually recursive bindings from JSON result in success"

    let json = """
      {
        "f": [
          "Abs",
          "x",
          [
            "App",
            [
              "Var",
              "Example.g"
            ],
            [
              "Var",
              "x"
            ]
          ]
        ],
        "g": [
          "Abs",
          "x",
          [
            "App",
            [
              "Var",
              "Example.f"
            ],
            [
              "Var",
              "x"
            ]
          ]
        ]
      }
    """

    expectSuccess description (readBindJSON json) \x -> do
      let fIdent = Ident "f"
      let fModuleName = Just (ModuleName "Example")
      let fQualified = Qualified fModuleName (Ident "g")
      let fAppVar1 = Var unit fQualified
      let fAppVar2 = Var unit (Qualified Nothing (Ident "x"))
      let fApp = App unit fAppVar1 fAppVar2
      let fAbs = Abs unit (Ident "x") fApp
      let fBinding = Tuple (Tuple unit fIdent) fAbs

      let gIdent = Ident "g"
      let gModuleName = Just (ModuleName "Example")
      let gQualified = Qualified gModuleName (Ident "f")
      let gAppVar1 = Var unit gQualified
      let gAppVar2 = Var unit (Qualified Nothing (Ident "x"))
      let gApp = App unit gAppVar1 gAppVar2
      let gAbs = Abs unit (Ident "x") gApp
      let gBinding = Tuple (Tuple unit gIdent) gAbs

      assertEqual x (Bind [fBinding, gBinding])

testCaseAlternatives :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
testCaseAlternatives = do
  log ""
  log "Test CaseAlternative"

  testCaseAlternative

  where

  -- |
  -- CaseAlternative
  --
  testCaseAlternative = do
    let description = "CaseAlternative from JSON result in success"

    let json = """
      [
        [
          [
            "ConstructorBinder",
            "Data.Identity.Identity",
            "Data.Identity.Identity",
            [
              [
                "VarBinder",
                "a"
              ]
            ]
          ]
        ],
        [
          "Var",
          "a"
        ]
      ]
    """

    expectSuccess description (readCaseAlternativeJSON json) \x -> do
      let moduleName = Just (ModuleName "Data.Identity")
      let type' = Qualified moduleName (ProperName "Identity")
      let constructor = Qualified moduleName (ProperName "Identity")
      let binder = VarBinder unit (Ident "a")
      let binders = [ConstructorBinder unit type' constructor [binder]]
      let result = Right (Var unit (Qualified Nothing (Ident "a")))

      assertEqual x (CaseAlternative {binders, result})

testBinders :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION | e) Unit
testBinders = do
  log ""
  log "Test Binder"

  testNullBinder
  testLiteralBinder
  testVarBinder
  testConstructorBinder
  testNamedBinder

  where

  -- |
  -- NullBinder
  --
  testNullBinder = do
    let description = "NullBinder from JSON result in success"

    let json = """
      "NullBinder"
    """

    expectSuccess description (readBinderJSON json) \x -> do

      assertEqual x (NullBinder unit)

  -- |
  -- LiteralBinder
  --
  testLiteralBinder = do
    let description = "LiteralBinder from JSON result in success"

    let json = """
      [
        "LiteralBinder",
        [
          "BooleanLiteral",
          true
        ]
      ]
    """

    expectSuccess description (readBinderJSON json) \x -> do
      let literal = BooleanLiteral true

      assertEqual x (LiteralBinder unit literal)

  -- |
  -- VarBinder
  --
  testVarBinder = do
    let description = "VarBinder from JSON result in success"

    let json = """
      [
        "VarBinder",
        "x"
      ]
    """

    expectSuccess description (readBinderJSON json) \x -> do
      let ident = Ident "x"

      assertEqual x (VarBinder unit ident)

  -- |
  -- ConstructorBinder
  --
  testConstructorBinder = do
    let description = "ConstructorBinder from JSON result in success"

    let json = """
      [
        "ConstructorBinder",
        "Data.Either.Either",
        "Data.Either.Left",
        [
          "NullBinder"
        ]
      ]
    """

    expectSuccess description (readBinderJSON json) \x -> do
      let moduleName = Just (ModuleName "Data.Either")
      let type' = Qualified moduleName (ProperName "Either")
      let constructor = Qualified moduleName (ProperName "Left")
      let binders = [NullBinder unit]

      assertEqual x (ConstructorBinder unit type' constructor binders)

  -- |
  -- NamedBinder
  --
  testNamedBinder = do
    let description = "NamedBinder from JSON result in success"

    let json = """
      [
        "NamedBinder",
        "x",
        "NullBinder"
      ]
    """

    expectSuccess description (readBinderJSON json) \x -> do
      let ident = Ident "x"
      let binder = NullBinder unit

      assertEqual x (NamedBinder unit ident binder)
