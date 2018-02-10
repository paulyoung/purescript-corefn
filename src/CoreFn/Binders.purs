-- |
-- The core functional representation for binders
--
module CoreFn.Binders where

import Prelude

import CoreFn.Ident (Ident)
import CoreFn.Literal (Literal)
import CoreFn.Names (ProperName, Qualified)
import Data.Array (intercalate)

-- |
-- Data type for binders
--
data Binder a
  -- |
  -- Wildcard binder
  --
  = NullBinder a
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder a (Literal (Binder a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder a (Qualified ProperName) (Qualified ProperName) (Array (Binder a))
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder a)

derive instance eqBinder :: Eq a => Eq (Binder a)
derive instance functorBinder :: Functor Binder
derive instance ordBinder :: Ord a => Ord (Binder a)

instance showBinder :: Show a => Show (Binder a) where
  show (NullBinder a) = "(NullBinder " <> show a <> ")"
  show (LiteralBinder a ls) =
    "(LiteralBinder " <>
      intercalate " " [ show a, show ls ] <>
    ")"
  show (VarBinder a i) =
    "(VarBinder " <>
      intercalate " " [ show a, show i ] <>
    ")"
  show (ConstructorBinder a t c bs) =
    "(ConstructorBinder " <>
      intercalate " " [ show a, show t, show c, show bs ] <>
    ")"
  show (NamedBinder a i b) =
    "(NamedBinder " <>
      intercalate " " [ show a, show i, show b ] <>
    ")"
