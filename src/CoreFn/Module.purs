module CoreFn.Module
  ( Module(..)
  , readModule
  , readModuleJSON
  ) where

import Prelude

import CoreFn.Expr (Bind, readBind)
import CoreFn.Ident (Ident, readIdent)
import CoreFn.Names (ModuleName(..), readModuleName)
import CoreFn.Util (objectProp)
import Data.Foreign (F, Foreign, readArray, readString)
import Data.Foreign.Index (class Index, index, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Traversable (traverse)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { builtWith :: String
  , moduleDecls :: Array (Bind a)
  , moduleExports :: Array Ident
  , moduleForeign :: Array Ident
  , moduleImports :: Array ModuleName
  , moduleName :: ModuleName
  }

derive instance eqModule :: Eq a => Eq (Module a)
derive instance ordModule :: Ord a => Ord (Module a)
derive instance functorModule :: Functor Module

instance showModule :: Show a => Show (Module a) where
  show (Module { builtWith
               , moduleDecls
               , moduleExports
               , moduleForeign
               , moduleImports
               , moduleName }) =
       "(Module { builtWith: " <> show builtWith <>
               ", moduleName: " <> show moduleName <>
               ", moduleDecls: " <> show moduleDecls <>
               ", moduleExports: " <> show moduleExports <>
               ", moduleForeign: " <> show moduleForeign <>
               ", moduleImports: " <> show moduleImports <>
               "})"

readModule :: Foreign -> F (Module Unit)
readModule x = do
  o <- objectProp "Module name not found" x

  builtWith     <- readProp "builtWith" o.value >>= readString
  moduleDecls   <- traverseArrayProp "decls"   o.value readBind
  moduleExports <- traverseArrayProp "exports" o.value readIdent
  moduleForeign <- traverseArrayProp "foreign" o.value readIdent
  moduleImports <- traverseArrayProp "imports" o.value readModuleName

  let moduleName = ModuleName o.key

  pure $ Module
    { builtWith
    , moduleDecls
    , moduleExports
    , moduleForeign
    , moduleImports
    , moduleName
    }

  where

  traverseArrayProp
    :: forall a b
     . (Index a)
    => a
    -> Foreign
    -> (Foreign -> F b)
    -> F (Array b)
  traverseArrayProp i value f = index value i >>= readArray >>= traverse f

readModuleJSON :: String -> F (Module Unit)
readModuleJSON = parseJSON >=> readModule
