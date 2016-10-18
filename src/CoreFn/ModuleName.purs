module CoreFn.ModuleName
  ( ModuleName(..)
  ) where

import Data.Generic (class Generic, gCompare, gEq, gShow)
import Prelude (class Eq, class Ord, class Show)

newtype ModuleName = ModuleName String

derive instance genericModuleName :: Generic ModuleName

instance showModuleName :: Show ModuleName where
  show = gShow

instance eqPosition :: Eq ModuleName where
  eq = gEq

instance ordModuleName :: Ord ModuleName where
  compare = gCompare
