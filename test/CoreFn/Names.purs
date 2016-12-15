module Test.CoreFn.Names
  ( testNames
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except.Trans (ExceptT)
import CoreFn.Names (ModuleName(..), OpName(..), ProperName(..), Qualified(..), readModuleNameJSON, readOpNameJSON)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Identity (Identity)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Test.Util (assertEqual, expectSuccess)

testNames :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testNames = do
  log ""
  log "Test Names"

  testModuleName
  testOpName
  testProperName
  testQualifiedOpNameWithoutModuleName
  testQualifiedOpNameWithModuleName
  testQualifiedProperNameWithoutModuleName
  testQualifiedProperNameWithModuleName

  where

  -- |
  -- ModuleName
  --
  testModuleName = do
    let description = "ModuleName from JSON results in success"

    let json = """
      "Main"
    """

    expectSuccess description (readModuleNameJSON json) \x ->
      assertEqual x (ModuleName "Main")

  -- |
  -- OpName
  --
  testOpName = do
    let description = "OpName from JSON results in success"

    let json = """
      "Control.Bind.bind"
    """

    expectSuccess description (readOpNameJSON json) \x ->
      assertEqual x (OpName "Control.Bind.bind")

  -- |
  -- ProperName
  --
  testProperName = do
    let description = "ProperName from JSON results in success"

    let json = """
      "Nothing"
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity ProperName

    expectSuccess description result \x ->
      assertEqual x (ProperName "Nothing")

  -- |
  -- Qualified
  --
  testQualifiedOpNameWithoutModuleName = do
    let description = "Qualified OpName without module name from JSON results in success"

    let json = """
      "bind"
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Qualified OpName)

    expectSuccess description result \(Qualified x y) -> do
      assertEqual x Nothing
      assertEqual y (OpName "bind")

  testQualifiedOpNameWithModuleName = do
    let description = "Qualified OpName with module name from JSON results in success"

    let json = """
      "Control.Bind.bind"
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Qualified OpName)

    expectSuccess description result \(Qualified x y) -> do
      assertEqual x (Just $ ModuleName "Control.Bind")
      assertEqual y (OpName "bind")

  testQualifiedProperNameWithoutModuleName = do
    let description = "Qualified ProperName without module name from JSON results in success"

    let json = """
      "Nothing"
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Qualified ProperName)

    expectSuccess description result \(Qualified x y) -> do
      assertEqual x Nothing
      assertEqual y (ProperName "Nothing")

  testQualifiedProperNameWithModuleName = do
    let description = "Qualified ProperName with module name from JSON results in success"

    let json = """
      "Data.Maybe.Nothing"
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity (Qualified ProperName)

    expectSuccess description result \(Qualified x y) -> do
      assertEqual x (Just $ ModuleName "Data.Maybe")
      assertEqual y (ProperName "Nothing")
