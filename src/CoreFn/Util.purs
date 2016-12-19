-- |
-- Utilities for working with the JSON representation of the functional core.
--
module CoreFn.Util
  ( foreignError
  ) where

import Prelude
import Control.Monad.Except.Trans (except)
import Data.Either (Either(..))
import Data.Foreign (ForeignError(..), F)
import Data.List.NonEmpty (singleton)

-- |
-- Create a `NonEmptyList` of a single `ForeignError` using the exception monad
-- transformer `ExceptT`.
--
foreignError :: forall a. String -> F a
foreignError = except <<< Left <<< singleton <<< ForeignError
