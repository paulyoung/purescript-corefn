module CoreFn.FromJSON where
  -- ( moduleFromJSON
  -- )

import Prelude

import CoreFn.Ann (Ann)
import CoreFn.Meta (ConstructorType(..))
import CoreFn.Module (FilePath(..), Module(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readString, typeOf)
import Data.Foreign.Index (readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Traversable (sequence, traverse)

-- constructorTypeFromJSON :: String -> F ConstructorType
-- constructorTypeFromJSON = parseJSON >=> constructorTypeFromJSON'
--   where
--   constructorTypeFromJSON' :: Foreign -> F ConstructorType
--   constructorTypeFromJSON' f = do
--     str <- readString f
--     case str of
--       "ProductType" -> pure ProductType
--       "SumType" -> pure SumType
--       _ -> fail $ ForeignError $ "Unknown ConstructorType: " <> str

-- metaFromJSON :: String -> F (Maybe Meta)
-- metaFromJSON f = parseJSON >=> metaFromJSON'
--   where
--   metaFromJSON' :: Foreign -> F (Maybe Meta)
--   metaFromJSON' f = do

-- annFromJSON

-- literalFromJSON

-- identFromJSON

-- properNameFromJSON

-- qualifiedFromJSON

-- moduleNameFromJSON

moduleFromJSON :: String -> F { version :: Version, module :: Module Ann }
moduleFromJSON = parseJSON >=> moduleFromJSON'
  where
  objectType :: String
  objectType = "object"

  moduleFromJSON' :: Foreign -> F { version :: Version, module :: Module Ann }
  moduleFromJSON' json
    | typ <- typeOf json, typ /= objectType = fail $ TypeMismatch objectType typ
  moduleFromJSON' json = do
    version <- map Version $ readProp "builtWith" json >>= readString

    moduleName <- map ModuleName $ readProp "moduleName" json
      >>= readArray
      >>= traverse (map ProperName <<< readString)

    modulePath <- map FilePath $ readProp "modulePath" json >>= readString

    moduleImports <- pure []
    moduleExports <- pure []
    moduleDecls <- pure []
    moduleForeign <- pure []
    moduleComments <- pure []

    pure
      { version
      , module: Module
        { moduleComments
        , moduleName
        , modulePath
        , moduleImports
        , moduleExports
        , moduleForeign
        , moduleDecls
        }
      }

-- bindFromJSON

-- recordFromJSON

-- exprFromJSON

-- caseAlternativeFromJSON

-- binderFromJSON
