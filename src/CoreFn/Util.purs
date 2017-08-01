-- |
-- Utilities for working with the JSON representation of the functional core.
--
module CoreFn.Util
  ( objectProp
  , objectProps
  ) where

import Prelude

import Control.Error.Util (exceptNoteA)
import Data.Array (head)
import Data.Foreign (F, Foreign, ForeignError(ForeignError))
import Data.Foreign.Index (readProp)
import Data.Foreign.Keys as K
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)
import Data.Traversable (traverse)

type Pair = { key :: String, value :: Foreign }

-- |
-- Create an array of records by reading a JSON object.
--
objectProps :: Foreign -> F (Array Pair)
objectProps x = do
  keys <- K.keys x
  traverse toPair keys

  where

  toPair :: String -> F Pair
  toPair key = do
    value <- readProp key x
    pure $ { key, value }

-- |
-- Create a record by reading a JSON object.
--
-- The object is expected to consist of a single key-value pair, where the key
-- is unknown.
--
-- The provided error message is used if a key cannot be obtained.
--
objectProp :: String -> Foreign -> F Pair
objectProp message x = do
  pairs <- objectProps x
  exceptNoteA ((Identity <<< head) pairs) (singleton (ForeignError message))
