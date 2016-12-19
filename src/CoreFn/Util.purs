-- |
-- Utilities for working with the JSON representation of the functional core.
--
module CoreFn.Util
  ( foreignError
  , readCoreFnLabel
  , readCoreFnValue
  ) where

import Prelude
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.Foreign (Foreign, ForeignError(..), F)
import Data.Foreign.Class (readProp)
import Data.List.NonEmpty (singleton)

-- |
-- Create a `NonEmptyList` of a single `ForeignError` using the exception monad
-- transformer `ExceptT`.
--
foreignError :: forall a. String -> F a
foreignError = except <<< Left <<< singleton <<< ForeignError

-- |
-- Read the label of a type in the JSON representation.
--
readCoreFnLabel :: Foreign -> F Foreign
readCoreFnLabel = readProp 0

-- |
-- Read the value of a type in the JSON representation.
--
readCoreFnValue :: Foreign -> F Foreign
readCoreFnValue = readProp 1
