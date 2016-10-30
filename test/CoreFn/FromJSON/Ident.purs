module Test.CoreFn.FromJSON.Ident
  ( testIdentFromJSON
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.FromJSON (identFromJSON)
import CoreFn.Ident (Ident(..))
import Test.Util (assertEqual, expectRight)

testIdentFromJSON :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION | e) Unit
testIdentFromJSON = do
  log ""
  log "Test CoreFn.FromJSON.Ident"

  -- |
  -- Ident
  --
  expectRight "Ident from JSON results in success" (identFromJSON """
    "main"
  """) \(x) ->
    assertEqual x (Ident "main")
