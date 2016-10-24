-- |
-- Defines the types of source code comments
--
module CoreFn.Comment
  ( Comment(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Foreign.Class (readProp, class IsForeign)
import Data.Generic (gCompare, gEq, gShow, class Generic)

data Comment
  = BlockComment String
  | LineComment String

derive instance genericComment :: Generic Comment

instance isForeignComment :: IsForeign Comment where
  read value = do
    BlockComment <$> readProp "BlockComment" value <|>
    LineComment <$> readProp "LineComment" value

instance showComment :: Show Comment where
  show = gShow

instance eqComment :: Eq Comment where
  eq = gEq

instance ordModule :: Ord Comment where
  compare = gCompare
