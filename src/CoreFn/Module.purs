module CoreFn.Module
  ( Module(..)
  ) where

import Prelude
import Control.Error.Util (note)
import CoreFn.Ident (Ident)
import CoreFn.Names (ModuleName(..))
import Data.Array as Array
import Data.Either (Either)
import Data.Foreign (Foreign, ForeignError(..))
import Data.Foreign.Class (readProp, class IsForeign)
import Data.Foreign.Index (prop)
import Data.Foreign.Keys (keys)
import Data.Generic (gCompare, gEq, gShow, class Generic)

-- |
-- The CoreFn module representation
--
data Module = Module
  { moduleExports :: Array Ident
  , moduleImports :: Array ModuleName
  , moduleName :: ModuleName
  }

derive instance genericModule :: Generic Module

instance isForeignModule :: IsForeign Module where
  read x = do
    let key = firstKey x
    value <- key >>= (flip prop) x
    moduleExports <- readProp "exports" value
    moduleImports <- readProp "imports" value
    moduleName <- ModuleName <$> key

    pure $ Module
      { moduleExports: moduleExports
      , moduleImports: moduleImports
      , moduleName: moduleName
      }

    where

    head :: forall a. Array a -> Either ForeignError a
    head = note (JSONError "Module name not found") <<< Array.head

    firstKey :: Foreign -> Either ForeignError String
    firstKey x = keys x >>= head

instance showModule :: Show Module where
  show = gShow

instance eqModule :: Eq Module where
  eq = gEq

instance ordModule :: Ord Module where
  compare = gCompare
