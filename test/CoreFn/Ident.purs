module Test.CoreFn.Ident
  ( testIdent
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Ident (Ident(..), readIdentJSON)
import Test.Util (assertEqual, expectSuccess)

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

    expectSuccess description (readIdentJSON json) \x ->
      assertEqual x (Ident "main")
