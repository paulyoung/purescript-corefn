module Test.CoreFn.Expr
  ( testExpr
  , testLiterals
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except.Trans (ExceptT)
import CoreFn.Expr (Expr(..), Literal(..))
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..))
import Data.Foreign.Class (readJSON)
import Data.Identity (Identity)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal String)

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal String)

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal String)

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal String)

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal String)

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal (Expr Unit))

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal (Expr Unit))

    expectSuccess description result \x ->
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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Literal String)

    expectFailure description result \x ->
      assertEqual x (singleton (ForeignError "Unknown literal: SomeLiteral"))

testExpr :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testExpr = do
  log ""
  log "Test Expr"

  testLiteralExpr
  testUnknownExpr

  where

  -- |
  -- Literal
  --
  testLiteralExpr = do
    let description = "StringLiteral from JSON results in success"

    let json = """
      [
        "Literal",
        [
          "StringLiteral",
          "Hello world!"
        ]
      ]
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Expr Unit)

    expectSuccess description result \x ->
      assertEqual x (Literal unit (StringLiteral "Hello world!"))

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

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Expr Unit)

    expectFailure description result \x ->
      assertEqual x (singleton (ForeignError "Unknown expression: SomeExpression"))

