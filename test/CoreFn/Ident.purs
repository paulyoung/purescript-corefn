module Test.CoreFn.Ident
  ( testIdent
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Ident (Ident(..))
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)
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

    let result = readJSON json :: Either ForeignError Ident

    expectRight description result \(x) ->
      assertEqual x (Ident "main")
