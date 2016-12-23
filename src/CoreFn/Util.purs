-- |
-- Utilities for working with the JSON representation of the functional core.
--
module CoreFn.Util
  ( objectProp
  ) where

import Prelude
import Data.Foreign.Keys as K
import Control.Error.Util (exceptNoteA)
import Data.Array (head)
import Data.Foreign (F, Foreign, ForeignError(ForeignError))
import Data.Foreign.Index (prop)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)

-- |
-- Create a record by reading a JSON object.
--
-- The object is expected to consist of a single key-value pair, where the key
-- is unknown.
--
-- The provided error message is used if a key cannot be obtained.
--
objectProp :: String -> Foreign -> F { key :: String, value :: Foreign }
objectProp message x = do
  keys <- K.keys x
  key <- exceptNoteA ((Identity <<< head) keys)
                      (singleton (ForeignError message))
  value <- prop key x
  pure $ { key, value }
