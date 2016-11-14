-- |
-- Names for value identifiers
--
module CoreFn.Ident
  ( Ident(..)
  ) where

import Prelude
import Data.Foreign (readString)
import Data.Foreign.Class (class IsForeign)
import Data.Generic (gShow, class Generic)
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

derive instance eqIdent :: Eq Ident
derive instance genericIdent :: Generic Ident
derive instance ordIdent :: Ord Ident

instance isForeignIdent :: IsForeign Ident where
  read value = Ident <$> readString value

instance showIdent :: Show Ident where
  show = gShow
