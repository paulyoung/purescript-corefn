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
import Data.Foreign.Index (prop)
import Data.Generic (gShow, class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (singleton)
import Data.Traversable (sequence)

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

  exportNames <- readProp "exports" value >>= readArray
  moduleExports <- sequence (readIdent <$> exportNames)

  foreignNames <- readProp "foreign" value >>= readArray
  moduleForeign <- sequence (readIdent <$> foreignNames)

  importNames <- readProp "imports" value >>= readArray
  moduleImports <- sequence (readModuleName <$> importNames)

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

readModuleJSON :: String -> F Module
readModuleJSON json = parseJSON json >>= readModule
