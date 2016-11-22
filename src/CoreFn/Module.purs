module CoreFn.Module
  ( Module(..)
  ) where

import Prelude
import Data.Array as Array
import Control.Error.Util (exceptNoteA)
import Control.Monad.Except.Trans (ExceptT)
import CoreFn.Ident (Ident)
import CoreFn.Names (ModuleName(..))
import Data.Foreign (Foreign, ForeignError(..))
import Data.Foreign.Class (readProp, class IsForeign)
import Data.Foreign.Index (prop)
import Data.Foreign.Keys (keys)
import Data.Generic (gShow, class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)

-- |
-- The CoreFn module representation
--
data Module = Module
  { moduleExports :: Array Ident
  , moduleForeign :: Array Ident
  , moduleImports :: Array ModuleName
  , moduleName :: ModuleName
  }

derive instance eqModule :: Eq Module
derive instance genericModule :: Generic Module
derive instance ordModule :: Ord Module

instance isForeignModule :: IsForeign Module where
  read x = do
    let key = firstKey x
    value <- key >>= (flip prop) x
    moduleExports <- readProp "exports" value
    moduleForeign <- readProp "foreign" value
    moduleImports <- readProp "imports" value
    moduleName <- ModuleName <$> key

    pure $ Module
      { moduleExports: moduleExports
      , moduleForeign: moduleForeign
      , moduleImports: moduleImports
      , moduleName: moduleName
      }

    where

    head :: forall a. Array a -> ExceptT (NonEmptyList ForeignError) Identity a
    head y = exceptNoteA ((Identity <<< Array.head) y)
                         (singleton (ForeignError "Module name not found"))

    firstKey :: Foreign -> ExceptT (NonEmptyList ForeignError) Identity String
    firstKey y = keys y >>= head

instance showModule :: Show Module where
  show = gShow
