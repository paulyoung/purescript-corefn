module CoreFn.Names
  ( ModuleName(..)
  , OpName(..)
  , ProperName(..)
  , Qualified(..)
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)

-- |
-- Module names
--
newtype ModuleName = ModuleName (Array ProperName)

derive instance eqModuleName :: Eq ModuleName
derive instance newtypeModuleName :: Newtype ModuleName _
derive instance ordModuleName :: Ord ModuleName

instance showModuleName :: Show ModuleName where
  show x = "(ModuleName " <> show (unwrap x) <> ")"


-- |
-- Operator alias names.
--
newtype OpName = OpName String

derive instance eqOpName :: Eq OpName
derive instance newtypeOpName :: Newtype OpName _
derive instance ordOpName :: Ord OpName

instance showOpName :: Show OpName where
  show x = "(OpName " <> show (unwrap x) <> ")"


-- |
-- Proper name, i.e. capitalized names for e.g. module names, type/data
-- constructors.
--
newtype ProperName = ProperName String

derive instance eqProperName :: Eq ProperName
derive instance newtypeProperName :: Newtype ProperName _
derive instance ordProperName :: Ord ProperName

instance showProperName :: Show ProperName where
  show x = "(ProperName " <> show (unwrap x) <> ")"


-- |
-- A qualified name, i.e. a name with an optional module name
--
data Qualified a = Qualified (Maybe ModuleName) a

derive instance eqQualified :: Eq a => Eq (Qualified a)
derive instance ordQualified :: Ord a => Ord (Qualified a)

instance showQualified :: Show a => Show (Qualified a) where
  show (Qualified m a) = "(Qualified " <> show m <> show a <> ")"
