module CoreFn.Module
  ( FilePath(..)
  , Module(..)
  , ModuleImport(..)
  , Version(..)
  ) where

import Prelude

import CoreFn.Ann (Comment, Ann)
import CoreFn.Expr (Bind)
import CoreFn.Ident (Ident)
import CoreFn.Names (ModuleName)
import Data.Newtype (class Newtype)

-- |
-- The CoreFn module representation
--
newtype Module a = Module
  { moduleComments :: Array Comment
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: Array ModuleImport
  , moduleExports :: Array Ident
  , moduleForeign :: Array Ident
  , moduleDecls :: Array (Bind a)
  }

derive instance newtypeModule :: Newtype (Module a) _
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
      ", moduleDecls: " <> show m.moduleDecls <> " " <>
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
      ", moduleName: " <> show moduleImport.moduleName <> " " <>
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
