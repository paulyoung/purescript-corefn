module CoreFn.Module
  ( FilePath(..)
  , Module(..)
  , ModuleImport(..)
  , Version(..)
  -- , readModule
  -- , readModuleJSON
  ) where

import Prelude

import CoreFn.Ann (Comment, Ann)
import CoreFn.Expr (Bind)
import CoreFn.Ident (Ident)
import CoreFn.Names (ModuleName(..))
import CoreFn.Util (objectProp)
import Data.Array (intercalate)
import Data.Foreign (F, Foreign, readArray, readString)
import Data.Foreign.Index (class Index, index, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleComments :: Array Comment
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: Array ModuleImport
  , moduleExports :: Array Ident
  , moduleForeign :: Array Ident
  , moduleDecls :: Array (Bind a)
  }

derive instance eqModule :: Eq a => Eq (Module a)
derive instance ordModule :: Ord a => Ord (Module a)

instance showModule :: Show a => Show (Module a) where
  show (Module m) =
    "(Module " <>
      "{ moduleComments: " <> show m.moduleComments <>
      ", moduleName: " <> show m.moduleName <>
      ", modulePath: " <> show m.modulePath <>
      ", moduleImports: " <> show m.moduleImports <>
      ", moduleExports: " <> show m.moduleExports <>
      ", moduleForeign: " <> show m.moduleForeign <>
      ", moduleDecls: " <> show m.moduleDecls <>
      "}" <>
    ")"


newtype ModuleImport = ModuleImport
  { ann :: Ann
  , moduleName :: ModuleName
  }

derive instance newtypeModuleImport :: Newtype ModuleImport _
derive instance eqModuleImport :: Eq ModuleImport
derive instance ordModuleImport :: Ord ModuleImport

instance showModuleImport :: Show ModuleImport where
  show (ModuleImport moduleImport) =
    "(ModuleImport " <>
      "{ ann: " <> show moduleImport.ann <>
      ", moduleName: " <> show moduleImport.moduleName <>
      "}" <>
    ")"

newtype Version = Version String

derive instance newtypeVersion :: Newtype Version _
derive newtype instance eqVersion :: Eq Version
derive newtype instance ordVersion :: Ord Version

instance showVersion :: Show Version where
  show (Version v) = "(Version " <> show v <> ")"


newtype FilePath = FilePath String

derive instance newtypeFilePath :: Newtype FilePath _
derive newtype instance eqFilePath :: Eq FilePath
derive newtype instance ordFilePath :: Ord FilePath

instance showFilePath :: Show FilePath where
  show (FilePath s) = "(FilePath " <> show s <> ")"


-- readModule :: Foreign -> F (Module Unit)
-- readModule x = do
--   o <- objectProp "Module name not found" x

--   builtWith     <- readProp "builtWith" o.value >>= readString
--   moduleDecls   <- traverseArrayProp "decls"   o.value readBind
--   moduleExports <- traverseArrayProp "exports" o.value readIdent
--   moduleForeign <- traverseArrayProp "foreign" o.value readIdent
--   moduleImports <- traverseArrayProp "imports" o.value readModuleName

--   let moduleName = ModuleName o.key

--   pure $ Module
--     { builtWith: Version builtWith
--     , moduleDecls
--     , moduleExports
--     , moduleForeign
--     , moduleImports
--     , moduleName
--     }

--   where

--   traverseArrayProp
--     :: forall a b
--      . (Index a)
--     => a
--     -> Foreign
--     -> (Foreign -> F b)
--     -> F (Array b)
--   traverseArrayProp i value f = index value i >>= readArray >>= traverse f

-- readModuleJSON :: String -> F (Module Unit)
-- readModuleJSON = parseJSON >=> readModule
