module CoreFn.Names
  ( ModuleName(..)
  , OpName(..)
  , ProperName(..)
  ) where

import Prelude
import Data.Foreign (readString)
import Data.Foreign.Class (class IsForeign)
import Data.Generic (class Generic, gCompare, gEq, gShow)

-- |
-- Module names
--
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

-- |
-- Operator alias names.
--
newtype OpName = OpName String

derive instance genericOpName :: Generic OpName

instance isForeignOpName :: IsForeign OpName where
  read value = OpName <$> readString value

instance showOpName :: Show OpName where
  show = gShow

instance eqOpName :: Eq OpName where
  eq = gEq

instance ordOpName :: Ord OpName where
  compare = gCompare

-- |
-- Proper name, i.e. capitalized names for e.g. module names, type/data
-- constructors.
--
newtype ProperName = ProperName String

derive instance genericProperName :: Generic ProperName

instance isForeignProperName :: IsForeign ProperName where
  read value = ProperName <$> readString value

instance showProperName :: Show ProperName where
  show = gShow

instance eqProperName :: Eq ProperName where
  eq = gEq

instance ordProperName :: Ord ProperName where
  compare = gCompare

