module CoreFn.ModuleName
  ( ModuleName(..)
  ) where

import Data.Foreign (readString)
import Data.Foreign.Class (class IsForeign)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Prelude ((<$>), class Eq, class Ord, class Show)

newtype ModuleName = ModuleName String

derive instance genericModuleName :: Generic ModuleName

instance isForeignModuleName :: IsForeign ModuleName where
  read value = ModuleName <$> readString value

instance showModuleName :: Show ModuleName where
  show = gShow

instance eqModuleName :: Eq ModuleName where
  eq = gEq

instance ordModuleName :: Ord ModuleName where
  compare = gCompare
