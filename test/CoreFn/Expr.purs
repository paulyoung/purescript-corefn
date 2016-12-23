module Test.CoreFn.Expr
  ( testExpr
  , testLiterals
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Expr (Expr(..), Literal(..), readExprJSON, readLiteralJSON)
import CoreFn.Ident (Ident(..))
import CoreFn.Names (ModuleName(..), Qualified(..))
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..))
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Util (assertEqual, expectFailure, expectSuccess)

testLiterals :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
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

testExpr :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testExpr = do
  log ""
  log "Test Expr"

  testLiteralExpr
  testAppExpr
  testVarExpr
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
      let expr1 = Var unit qualified
      let expr2 = Literal unit (StringLiteral "Hello world!")
      assertEqual x (App expr1 expr2)

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
