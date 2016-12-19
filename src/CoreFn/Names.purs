module CoreFn.Names
  ( ModuleName(..)
  , OpName(..)
  , ProperName(..)
  , Qualified(..)
  , readModuleName
  , readModuleNameJSON
  , readOpName
  , readOpNameJSON
  , readProperName
  , readProperNameJSON
  , readQualified
  , readQualifiedJSON
  ) where

import Prelude
import Control.Error.Util (exceptNoteM)
import Data.Array (init, last, null)
import Data.Foreign (F, Foreign, ForeignError(..), parseJSON, readString)
import Data.Generic (class Generic, gShow)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), joinWith, split)

-- |
-- Module names
--
newtype ModuleName = ModuleName String

derive instance eqModuleName :: Eq ModuleName
derive instance genericModuleName :: Generic ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance ordModuleName :: Ord ModuleName

instance showModuleName :: Show ModuleName where
  show = gShow

readModuleName :: Foreign -> F ModuleName
readModuleName x = ModuleName <$> readString x

readModuleNameJSON :: String -> F ModuleName
readModuleNameJSON json = parseJSON json >>= readModuleName

-- |
-- Operator alias names.
--
newtype OpName = OpName String

derive instance eqOpName :: Eq OpName
derive instance genericOpName :: Generic OpName
derive instance newtypeOpName :: Newtype OpName _
derive instance ordOpName :: Ord OpName

instance showOpName :: Show OpName where
  show = gShow

readOpName :: Foreign -> F OpName
readOpName x = OpName <$> readString x

readOpNameJSON :: String -> F OpName
readOpNameJSON json = parseJSON json >>= readOpName

-- |
-- Proper name, i.e. capitalized names for e.g. module names, type/data
-- constructors.
--
newtype ProperName = ProperName String

derive instance eqProperName :: Eq ProperName
derive instance genericProperName :: Generic ProperName
derive instance newtypeProperName :: Newtype ProperName _
derive instance ordProperName :: Ord ProperName

instance showProperName :: Show ProperName where
  show = gShow

readProperName :: Foreign -> F ProperName
readProperName x = ProperName <$> readString x

readProperNameJSON :: String -> F ProperName
readProperNameJSON json = parseJSON json >>= readProperName

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

derive instance eqQualified :: (Generic a, Eq a) => Eq (Qualified a)
derive instance genericQualified :: (Generic a) => Generic (Qualified a)
derive instance ordQualified :: (Generic a, Ord a) => Ord (Qualified a)

instance showQualified :: (Generic a, Show a) => Show (Qualified a) where
  show = gShow

readQualified :: forall a. (String -> a) -> Foreign -> F (Qualified a)
readQualified ctor x = readString x >>= (flip exceptNoteM errors) <<< toQualified

  where

  arrayToMaybe :: forall b. Array b -> Maybe (Array b)
  arrayToMaybe xs | null xs = Nothing
                  | otherwise = Just xs

  init' :: forall b. Array b -> Maybe (Array b)
  init' xs = init xs >>= arrayToMaybe

  delimiter = "."

  toModuleName :: Array String -> ModuleName
  toModuleName = ModuleName <<< (joinWith delimiter)

  toQualified :: String -> Maybe (Qualified a)
  toQualified s = do
    let parts = split (Pattern delimiter) s
    lastPart <- last parts
    let moduleName = toModuleName <$> init' parts
    Just $ Qualified moduleName (ctor lastPart)

  errors :: NonEmptyList ForeignError
  errors = singleton (ForeignError "Error parsing qualified name")

readQualifiedJSON :: forall a. (String -> a) -> String -> F (Qualified a)
readQualifiedJSON ctor json = parseJSON json >>= readQualified ctor
