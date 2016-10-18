module CoreFn.Module where

import Data.Array as Array
import Control.Error.Util (note)
import CoreFn.ModuleName (ModuleName(..))
import Data.Either (Either)
import Data.Foreign (Foreign, ForeignError(..))
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Keys (keys)
import Data.Generic (class Generic, gCompare, gEq, gShow)
import Prelude ((<$>), (>>=), (<<<), class Eq, class Ord, class Show)

-- |
-- The CoreFn module representation
--
data Module = Module
  { moduleName :: ModuleName
  }

derive instance genericModule :: Generic Module

instance isForeignModule :: IsForeign Module where
  read x = Module <<< { moduleName: _ } <<< ModuleName <$> firstKey x
    where

    head :: forall a. Array a -> Either ForeignError a
    head = note (JSONError "Module name not found") <<< Array.head

    firstKey :: Foreign -> Either ForeignError String
    firstKey x = keys x >>= head

instance showModule :: Show Module where
  show = gShow

instance eqPosition :: Eq Module where
  eq = gEq

instance ordModule :: Ord Module where
  compare = gCompare
