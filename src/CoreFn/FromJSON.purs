module CoreFn.FromJSON
  ( moduleFromJSON
  ) where

import CoreFn.Module (Module)
import Data.Either (Either)
import Data.Foreign (ForeignError)
import Data.Foreign.Class (readJSON)

moduleFromJSON :: String -> Either ForeignError Module
moduleFromJSON = readJSON
