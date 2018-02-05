module CoreFn.Module
  ( FilePath(..)
  , Module(..)
  , Version(..)
  -- , readModule
  -- , readModuleJSON
  ) where

import Prelude

import CoreFn.Ann (Comment)
import CoreFn.Expr (Bind)
import CoreFn.Ident (Ident)
import CoreFn.Names (ModuleName(..))
import CoreFn.Util (objectProp)
import Data.Foreign (F, Foreign, readArray, readString)
import Data.Foreign.Index (class Index, index, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Traversable (traverse)

-- |
-- The CoreFn module representation
--
data Module a = Module
  { moduleComments :: Array Comment
  , moduleName :: ModuleName
  , modulePath :: FilePath
  , moduleImports :: Array ModuleName
  , moduleExports :: Array Ident
  , moduleForeign :: Array Ident
  , moduleDecls :: Array (Bind a)
  }

derive instance eqModule :: Eq a => Eq (Module a)
derive instance ordModule :: Ord a => Ord (Module a)

instance showModule :: Show a => Show (Module a) where
  show (Module m) =
    "(Module " <>
      "{ moduleComments: " <> show moduleComments <>
      ", moduleName: " <> show moduleName <>
      ", modulePath: " <> show modulePath <>
      ", moduleImports: " <> show moduleImports <>
      ", moduleExports: " <> show moduleExports <>
      ", moduleForeign: " <> show moduleForeign <>
      ", moduleDecls: " <> show moduleDecls <>
      "}" <>
    ")"
    where
      { moduleComments
      , moduleName
      , modulePath
      , moduleImports
      , moduleExports
      , moduleForeign
      , moduleDecls
      } = m


newtype Version = Version String

derive newtype instance eqVersion :: Eq Version
derive newtype instance ordVersion :: Ord Version

instance showVersion :: Show Version where
  show (Version v) = "(Version " <> show v <> ")"


newtype FilePath = FilePath String

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
