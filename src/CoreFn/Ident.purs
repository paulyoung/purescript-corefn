-- |
-- Names for value identifiers
--
module CoreFn.Ident
  ( Ident(..)
  , readIdent
  , readIdentJSON
  ) where

import Prelude

import Data.Foreign (F, Foreign, readString)
import Data.Foreign.JSON (parseJSON)
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

instance showIdent :: Show Ident where
  show = gShow

readIdent :: Foreign -> F Ident
readIdent x = Ident <$> readString x

readIdentJSON :: String -> F Ident
readIdentJSON = parseJSON >=> readIdent
