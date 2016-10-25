-- |
-- Names for value identifiers
--
module CoreFn.Ident
  ( Ident(..)
  ) where

import Prelude
import Data.Foreign (readString)
import Data.Foreign.Class (class IsForeign)
import Data.Generic (gCompare, gEq, gShow, class Generic)
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

derive instance genericIdent :: Generic Ident

instance isForeignIdent :: IsForeign Ident where
  read value = Ident <$> readString value

instance showIdent :: Show Ident where
  show = gShow

instance eqIdent :: Eq Ident where
  eq = gEq

instance ordIdent :: Ord Ident where
  compare = gCompare
