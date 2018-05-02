module CoreFn.Meta where

import Prelude

import CoreFn.Ident (Ident)

-- |
-- Metadata annotations
--
data Meta
  -- |
  -- The contained value is a data constructor
  --
  = IsConstructor ConstructorType (Array Ident)
  -- |
  -- The contained value is a newtype
  --
  | IsNewtype
  -- |
  -- The contained value is a typeclass dictionary constructor
  --
  | IsTypeClassConstructor
  -- |
  -- The contained reference is for a foreign member
  --
  | IsForeign
  -- |
  -- The contained value is a where clause
  --
  | IsWhere

derive instance eqMeta :: Eq Meta
derive instance ordMeta :: Ord Meta

instance showMeta :: Show Meta where
  show (IsConstructor c is) =
    "(IsConstructor " <> show c <> " " <> show is <> ")"
  show IsNewtype = "IsNewtype"
  show IsTypeClassConstructor = "IsTypeClassConstructor"
  show IsForeign = "IsForeign"


-- |
-- Data constructor metadata
--
data ConstructorType
  -- |
  -- The constructor is for a type with a single constructor
  --
  = ProductType
  -- |
  -- The constructor is for a type with multiple constructors
  --
  | SumType

derive instance eqConstructorType :: Eq ConstructorType
derive instance ordConstructorType :: Ord ConstructorType

instance showConstructorType :: Show ConstructorType where
  show ProductType = "ProductType"
  show SumType = "SumType"
