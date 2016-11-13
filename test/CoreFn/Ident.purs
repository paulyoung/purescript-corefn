module Test.CoreFn.Ident
  ( testIdent
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT)
import CoreFn.Ident (Ident(..))
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)
import Data.Identity (Identity)
import Data.List.Types (NonEmptyList)
import Test.Util (assertEqual, expectRight)

testIdent :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testIdent = do
  log ""
  log "Test Ident"

  testIdentConstructor

  where

  -- |
  -- Ident
  --
  testIdentConstructor = do
    let description = "Ident from JSON results in success"

    let json = """
      "main"
    """

    let result = readJSON json :: ExceptT (NonEmptyList ForeignError) Identity Ident

    expectRight description (runExcept result) \(x) ->
      assertEqual x (Ident "main")
