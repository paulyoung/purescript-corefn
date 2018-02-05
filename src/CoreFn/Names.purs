module CoreFn.Names
  ( ModuleName(..)
  , OpName(..)
  , ProperName(..)
  , Qualified(..)
  -- , readModuleName
  -- , readModuleNameJSON
  -- , readOpName
  -- , readOpNameJSON
  -- , readProperName
  -- , readProperNameJSON
  -- , readQualified
  -- , readQualifiedJSON
  ) where

import Prelude

import Control.Error.Util (exceptNoteM)
import Data.Array (init, last, null)
import Data.Foreign (F, Foreign, ForeignError(..), readString)
import Data.Foreign.JSON (parseJSON)
import Data.Generic (class Generic, gShow)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), joinWith, split)

-- |
-- Module names
--
newtype ModuleName = ModuleName (Array ProperName)

derive instance eqModuleName :: Eq ModuleName
derive instance genericModuleName :: Generic ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance ordModuleName :: Ord ModuleName
derive newtype instance showModuleName :: Show ModuleName

-- readModuleName :: Foreign -> F ModuleName
-- readModuleName x = ModuleName <$> readString x

-- readModuleNameJSON :: String -> F ModuleName
-- readModuleNameJSON = parseJSON >=> readModuleName

-- |
-- Operator alias names.
--
newtype OpName = OpName String

derive instance eqOpName :: Eq OpName
derive instance genericOpName :: Generic OpName
derive instance newtypeOpName :: Newtype OpName _
derive instance ordOpName :: Ord OpName
derive newtype instance showOpName :: Show OpName

-- readOpName :: Foreign -> F OpName
-- readOpName x = OpName <$> readString x

-- readOpNameJSON :: String -> F OpName
-- readOpNameJSON = parseJSON >=> readOpName

-- |
-- Proper name, i.e. capitalized names for e.g. module names, type/data
-- constructors.
--
newtype ProperName = ProperName String

derive instance eqProperName :: Eq ProperName
derive instance genericProperName :: Generic ProperName
derive instance newtypeProperName :: Newtype ProperName _
derive instance ordProperName :: Ord ProperName
derive newtype instance showProperName :: Show ProperName

-- readProperName :: Foreign -> F ProperName
-- readProperName x = ProperName <$> readString x

-- readProperNameJSON :: String -> F ProperName
-- readProperNameJSON = parseJSON >=> readProperName

-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

derive instance eqQualified :: (Generic a, Eq a) => Eq (Qualified a)
-- derive instance genericQualified :: (Generic a) => Generic (Qualified a)
derive instance ordQualified :: (Generic a, Ord a) => Ord (Qualified a)

-- instance showQualified :: (Generic a, Show a) => Show (Qualified a) where
--   show = gShow

instance showQualified :: Show a => Show (Qualified a) where
  show (Qualified m a) = "(Qualified " <> show m <> show a <> ")"


-- readQualified :: forall a. (String -> a) -> Foreign -> F (Qualified a)
-- readQualified ctor = readString >=> toQualified ctor

--   where

--   arrayToMaybe :: forall b. Array b -> Maybe (Array b)
--   arrayToMaybe xs | null xs = Nothing
--                   | otherwise = Just xs

--   init' :: forall b. Array b -> Maybe (Array b)
--   init' = init >=> arrayToMaybe

--   delimiter = "."

--   toModuleName :: Array String -> ModuleName
--   toModuleName = ModuleName <<< (joinWith delimiter)

--   toQualified' :: (String -> a) -> String -> Maybe (Qualified a)
--   toQualified' c s = do
--     let parts = split (Pattern delimiter) s
--     lastPart <- last parts
--     let moduleName = toModuleName <$> init' parts
--     Just $ Qualified moduleName (c lastPart)

--   toQualified :: (String -> a) -> String -> F (Qualified a)
--   toQualified c s = exceptNoteM (toQualified' c s) errors

--   errors :: NonEmptyList ForeignError
--   errors = singleton (ForeignError "Error parsing qualified name")

-- readQualifiedJSON :: forall a. (String -> a) -> String -> F (Qualified a)
-- readQualifiedJSON ctor = parseJSON >=> readQualified ctor
