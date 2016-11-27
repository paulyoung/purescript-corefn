-- |
-- Utilities for working with the JSON representation of the functional core.
--
module CoreFn.Util
  ( mapCoreFnValue
  , readCoreFnLabel
  , readCoreFnValue
  , unrecognizedLabel
  ) where

import Prelude
import Control.Monad.Except.Trans (ExceptT, except)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(..))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Identity (Identity)
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)

-- |
-- Read the label of a type in the JSON representation.
--
readCoreFnLabel
  :: forall a
   . (IsForeign a)
  => Foreign
  -> ExceptT (NonEmptyList ForeignError) Identity a
readCoreFnLabel = readProp 0

-- |
-- Read the value of a type in the JSON representation.
--
readCoreFnValue
  :: forall b
   . (IsForeign b)
  => Foreign
  -> ExceptT (NonEmptyList ForeignError) Identity b
readCoreFnValue = readProp 1

-- |
-- Map the value of a type in the JSON representation.
--
mapCoreFnValue
  :: forall a b
   . (IsForeign a)
  => (a -> b)
  -> Foreign
  -> ExceptT (NonEmptyList ForeignError) Identity b
mapCoreFnValue f = map f <<< readCoreFnValue


unrecognizedLabel
  :: forall a b c d
   . (Applicative d)
  => a
  -> b
  -> String
  -> ExceptT (NonEmptyList ForeignError) d c
unrecognizedLabel label _ = except <<< Left <<< singleton <<< ForeignError
