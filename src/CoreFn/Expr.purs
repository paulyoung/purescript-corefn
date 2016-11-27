-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Expr(..)
  , Literal(..)
  ) where

import Prelude
import Control.Monad.Except.Trans (ExceptT)
import CoreFn.Util (mapCoreFnValue, readCoreFnLabel, readCoreFnValue, unrecognizedLabel)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Keys as K
import Data.Generic (class Generic, gShow)
import Data.Identity (Identity)
import Data.List.Types (NonEmptyList)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Int Number)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Boolean
  -- |
  -- An array literal
  --
  | ArrayLiteral (Array a)
  -- |
  -- An object literal
  --
  | ObjectLiteral (Array (Tuple String a))

derive instance eqLiteral :: (Generic a, Eq a) => Eq (Literal a)
derive instance genericLiteral :: Generic a => Generic (Literal a)
derive instance ordLiteral :: (Generic a, Ord a) => Ord (Literal a)

instance isForeignLiteral :: (IsForeign a) => IsForeign (Literal a) where
  read value = readCoreFnLabel value >>= flip readLiteral value

    where

    readValues
      :: forall b
       . (IsForeign b)
      => Array Foreign
      -> ExceptT (NonEmptyList ForeignError) Identity (Array b)
    readValues = sequence <<< map read

    readPair
      :: Foreign
      -> String
      -> ExceptT (NonEmptyList ForeignError) Identity (Tuple String a)
    readPair obj key = Tuple key <$> readProp key obj

    readPairs
      :: Foreign
      -> Array String
      -> ExceptT (NonEmptyList ForeignError) Identity (Array (Tuple String a))
    readPairs obj = sequence <<< (map <<< readPair) obj

    readLiteral
      :: String
      -> Foreign
      -> ExceptT (NonEmptyList ForeignError) Identity (Literal a)
    readLiteral "IntLiteral" = mapCoreFnValue (NumericLiteral <<< Left)
    readLiteral "NumberLiteral" = mapCoreFnValue (NumericLiteral <<< Right)
    readLiteral "StringLiteral" = mapCoreFnValue StringLiteral
    readLiteral "CharLiteral" = mapCoreFnValue CharLiteral
    readLiteral "BooleanLiteral" = mapCoreFnValue BooleanLiteral
    readLiteral "ArrayLiteral" = \v -> do
      array <- readCoreFnValue v
      ArrayLiteral <$> readValues array
    readLiteral "ObjectLiteral" = \v -> do
      obj <- readCoreFnValue v
      keys <- K.keys obj
      ObjectLiteral <$> readPairs obj keys
    readLiteral label = \v ->
      unrecognizedLabel label v ("Unknown literal: " <> label)

instance showLiteral :: (Generic a, Show a) => Show (Literal a) where
  show = gShow

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))

derive instance eqExpr :: (Generic a, Eq a) => Eq (Expr a)
derive instance genericExpr :: Generic a => Generic (Expr a)
derive instance ordExpr :: (Generic a, Ord a) => Ord (Expr a)

instance isForeignExpr :: IsForeign (Expr Unit) where
  read value = readCoreFnLabel value >>= flip readExpr value

    where

    readExpr :: String -> Foreign -> ExceptT (NonEmptyList ForeignError) Identity (Expr Unit)
    readExpr "Literal" = mapCoreFnValue (Literal unit)
    readExpr label = \v ->
      unrecognizedLabel label v ("Unknown expression: " <> label)

instance showExpr :: (Generic a, Show a) => Show (Expr a) where
  show = gShow
