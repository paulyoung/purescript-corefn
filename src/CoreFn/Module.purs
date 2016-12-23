module CoreFn.Module
  ( Module(..)
  , readModule
  , readModuleJSON
  ) where

import Prelude
import CoreFn.Ident (Ident, readIdent)
import CoreFn.Names (ModuleName(..), readModuleName)
import CoreFn.Util (objectProp)
import Data.Foreign (F, Foreign, parseJSON, readArray)
import Data.Foreign.Class (readProp)
import Data.Generic (gShow, class Generic)
import Data.Foreign.Index (class Index)
import Data.Traversable (traverse)

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

instance showModule :: Show Module where
  show = gShow

readModule :: Foreign -> F Module
readModule x = do
  o <- objectProp "Module name not found" x

  moduleExports <- traverseArrayProp "exports" o.value readIdent
  moduleForeign <- traverseArrayProp "foreign" o.value readIdent
  moduleImports <- traverseArrayProp "imports" o.value readModuleName

  let moduleName = ModuleName o.key

  pure $ Module
    { moduleExports: moduleExports
    , moduleForeign: moduleForeign
    , moduleImports: moduleImports
    , moduleName: moduleName
    }

  where

  traverseArrayProp
    :: forall a i
     . (Index i)
    => i
    -> Foreign
    -> (Foreign -> F a)
    -> F (Array a)
  traverseArrayProp prop value f = readProp prop value >>= readArray >>= traverse f

readModuleJSON :: String -> F Module
readModuleJSON json = parseJSON json >>= readModule
