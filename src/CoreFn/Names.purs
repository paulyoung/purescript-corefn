module CoreFn.Names
  ( ModuleName(..)
  , OpName(..)
  , ProperName(..)
  , Qualified(..)
  ) where

import Prelude
import Control.Error.Util (exceptNoteM)
import Data.Array (init, last, null)
import Data.Foreign (readString, ForeignError(ForeignError))
import Data.Foreign.Class (class IsForeign)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.String (Pattern(..), joinWith, split)

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
derive instance newtypeOpName :: Newtype OpName _

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
derive instance newtypeProperName :: Newtype ProperName _

instance isForeignProperName :: IsForeign ProperName where
  read value = ProperName <$> readString value

instance showProperName :: Show ProperName where
  show = gShow

instance eqProperName :: Eq ProperName where
  eq = gEq

instance ordProperName :: Ord ProperName where
  compare = gCompare

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

derive instance genericQualified :: (Generic a) => Generic (Qualified a)

instance isForeignQualified :: Newtype t String => IsForeign (Qualified t) where
  read value = readString value >>= (flip exceptNoteM errors) <<< toQualified

    where

    arrayToMaybe :: forall a. Array a -> Maybe (Array a)
    arrayToMaybe xs | null xs = Nothing
                    | otherwise = Just xs

    init' :: forall a. Array a -> Maybe (Array a)
    init' xs = init xs >>= arrayToMaybe

    delimiter = "."

    toModuleName :: Array String -> ModuleName
    toModuleName = ModuleName <<< (joinWith delimiter)

    toQualified :: Newtype t String => String -> Maybe (Qualified t)
    toQualified s = do
      let parts = split (Pattern delimiter) s
      Qualified (toModuleName <$> init' parts) <<< wrap <$> last parts

    errors :: NonEmptyList ForeignError
    errors = singleton (ForeignError "Error parsing qualified name")

instance showQualified :: (Generic a, Show a) => Show (Qualified a) where
  show = gShow

instance eqQualified :: (Generic a, Eq a) => Eq (Qualified a) where
  eq = gEq

instance ordQualified :: (Generic a, Ord a) => Ord (Qualified a) where
  compare = gCompare
