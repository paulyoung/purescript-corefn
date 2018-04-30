module Test.Parsing where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import CoreFn.Module (Module(..), readModuleJSON)
import CoreFn.Names (ModuleName(..))
import Data.Foldable (for_)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile, readdir)
import Node.Path (concat)
import Test.Util (assertEqual, expectSuccess)

testParsing :: forall e. Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | e) Unit
testParsing = do
  log ""
  log "Test parsing modules"

  files <- readdir "output"
  for_ files \file -> do
    let description = "Parsing " <> file <> " results in success"

    json <- readTextFile UTF8 (concat ["output", file, "corefn.json"])

    expectSuccess description (readModuleJSON json) \(Module {moduleName}) ->
      assertEqual moduleName (ModuleName file)
