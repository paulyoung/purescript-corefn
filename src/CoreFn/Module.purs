module CoreFn.Module
  ( Module(..)
  , readModule
  , readModuleJSON
  ) where

import Prelude
import Data.Array as Array
import Data.Foreign.Keys as K
import Control.Error.Util (exceptNoteA)
import CoreFn.Ident (Ident, readIdent)
import CoreFn.Names (ModuleName(..), readModuleName)
import Data.Foreign (F, Foreign, ForeignError(..), parseJSON, readArray)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (class Index, prop)
import Data.Generic (gShow, class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)
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
  keys <- K.keys x
  key <- head keys
  value <- prop key x

  moduleExports <- traverseArrayProp "exports" value readIdent
  moduleForeign <- traverseArrayProp "foreign" value readIdent
  moduleImports <- traverseArrayProp "imports" value readModuleName

  let moduleName = ModuleName key

  pure $ Module
    { moduleExports: moduleExports
    , moduleForeign: moduleForeign
    , moduleImports: moduleImports
    , moduleName: moduleName
    }

  where

  head :: Array ~> F
  head y = exceptNoteA ((Identity <<< Array.head) y)
                        (singleton (ForeignError "Module name not found"))

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
