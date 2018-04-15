-- |
-- Names for value identifiers
--
module CoreFn.Ident
  ( Ident(..)
  ) where

import Prelude

import Data.Maybe (Maybe)

data Ident
  -- |
  -- An alphanumeric identifier
  --
  = Ident String
  -- |
  -- A generated name for an identifier
  --
  | GenIdent (Maybe String) Int
  -- |
  -- A generated name used only for type-checking
  --
  | UnusedIdent

derive instance eqIdent :: Eq Ident
derive instance ordIdent :: Ord Ident

instance showIdent :: Show Ident where
  show (Ident s) = "(Ident " <> show s <> ")"
  show (GenIdent s i) = "(GenIdent " <> show s <> " " <> show i <> ")"
  show UnusedIdent = "UnusedIdent"
