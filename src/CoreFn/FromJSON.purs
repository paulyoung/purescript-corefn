module CoreFn.FromJSON where
  -- ( moduleFromJSON
  -- )

import Prelude

import Control.Alt ((<|>))
import Control.MonadPlus (class Plus, empty)
import CoreFn.Ann (Ann(..), Comment(..), SourcePos(..), SourceSpan(..))
import CoreFn.Expr (Bind(..), Expr, Bind')
import CoreFn.Ident (Ident(..))
import CoreFn.Meta (ConstructorType(..), Meta)
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..))
import Data.Array as Array
import Data.Foreign (F, Foreign, ForeignError(..), fail, isNull, readArray, readInt, readString, typeOf)
import Data.Foreign.Index (index, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.Keys (keys)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

objectType :: String
objectType = "object"

object :: forall a. (Foreign -> F a) -> Foreign -> F a
object _ json
  | typ <- typeOf json, typ /= objectType = fail $ TypeMismatch objectType typ
object f json = f json

nullable :: forall f a. Plus f => (Foreign -> F (f a)) -> Foreign -> F (f a)
nullable _ json | isNull json = pure empty
nullable f json = f json

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

metaFromJSON :: Foreign -> F (Maybe Meta)
metaFromJSON = nullable >>> object $ \json -> do
  -- type_ <-
  pure Nothing

annFromJSON :: FilePath -> Foreign -> F Ann
annFromJSON modulePath = object \json -> do
  sourceSpan <- readProp "sourceSpan" json >>= sourceSpanFromJSON
  meta <- readProp "meta" json >>= metaFromJSON
  pure $ Ann { sourceSpan, comments: [], type: Nothing, meta }
  where
  sourceSpanFromJSON :: Foreign -> F SourceSpan
  sourceSpanFromJSON = object \json -> do
    spanStart <- readProp "start" json >>= sourcePosFromJSON
    spanEnd <- readProp "end" json >>= sourcePosFromJSON
    pure $ SourceSpan { spanName: unwrap modulePath, spanStart, spanEnd }

  sourcePosFromJSON :: Foreign -> F SourcePos
  sourcePosFromJSON json = do
    sourcePosLine <- index json 0 >>= readInt
    sourcePosColumn <- index json 1 >>= readInt
    pure $ SourcePos { sourcePosLine, sourcePosColumn }

-- literalFromJSON

identFromJSON :: Foreign -> F Ident
identFromJSON = map Ident <<< readString

properNameFromJSON :: Foreign -> F ProperName
properNameFromJSON = map ProperName <<< readString

-- qualifiedFromJSON

moduleNameFromJSON :: Foreign -> F ModuleName
moduleNameFromJSON json = map ModuleName $ readArray json
  >>= traverse properNameFromJSON

moduleFromJSON :: String -> F { version :: Version, module :: Module Ann }
moduleFromJSON = parseJSON >=> moduleFromObj
  where
  moduleFromObj :: Foreign -> F { version :: Version, module :: Module Ann }
  moduleFromObj = object \json -> do
    version <- map Version $ readProp "builtWith" json >>= readString

    moduleName <- readProp "moduleName" json >>= moduleNameFromJSON

    modulePath <- map FilePath $ readProp "modulePath" json >>= readString

    moduleImports <- readProp "imports" json
      >>= readArray
      >>= traverse (importFromJSON modulePath)

    moduleExports <- readProp "exports" json
      >>= readArray
      >>= traverse identFromJSON

    moduleDecls <- pure []

    moduleForeign <- readProp "foreign" json
      >>= readArray
      >>= traverse identFromJSON

    moduleComments <- readProp "comments" json
      >>= readArray
      >>= traverse commentFromJSON

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

  importFromJSON
    :: FilePath
    -> Foreign
    -> F ModuleImport
  importFromJSON modulePath = object \json -> do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    moduleName <- readProp "moduleName" json >>= moduleNameFromJSON
    pure $ ModuleImport { ann,  moduleName }

  commentFromJSON :: Foreign -> F Comment
  commentFromJSON json =
    lineCommentFromJSON json
      <|> blockCommentFromJSON json
      <|> invalidComment json
    where
    blockCommentFromJSON :: Foreign -> F Comment
    blockCommentFromJSON =
      readProp "BlockComment" >=> map BlockComment <<< readString

    lineCommentFromJSON :: Foreign -> F Comment
    lineCommentFromJSON =
      readProp "LineComment" >=> map LineComment <<< readString

    invalidComment :: Foreign -> F Comment
    invalidComment = keys >=> Array.head >>> case _ of
      Just type_ -> fail $ ForeignError $ "Unknown Comment type: " <> type_
      Nothing -> fail $ ForeignError "Invalid Comment"

bindFromJSON :: FilePath -> Foreign -> F (Bind Ann)
bindFromJSON modulePath = object \json -> do
  type_ <- readProp "bindType" json >>= readString
  case type_ of
    "NonRec" -> NonRec <$> bindFromObj json
    "Rec" ->
      map Rec
        $ readProp "binds" json
        >>= readArray
        >>= traverse (object bindFromObj)
    _ -> fail $ ForeignError $ "Unknown Bind type: " <> type_
  where
  bindFromObj :: Foreign -> F (Bind' Ann)
  bindFromObj json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    ident <- readProp "identifier" json >>= identFromJSON
    -- expr <- readProp "expression" >>= exprFromJSON modulePath
    expr <- fail $ ForeignError "FIXME"
    pure $ Tuple (Tuple ann ident) expr

-- recordFromJSON

-- exprFromJSON

-- caseAlternativeFromJSON

-- binderFromJSON
