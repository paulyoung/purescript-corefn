-- |
-- Consume the core functional representation in JSON format
--
module CoreFn.FromJSON
  ( identFromJSON
  , moduleFromJSON
  ) where

import CoreFn.Ident (Ident)
import CoreFn.Module (Module)
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)

identFromJSON :: String -> Either ForeignError Ident
identFromJSON = readJSON

moduleFromJSON :: String -> Either ForeignError Module
moduleFromJSON = readJSON
