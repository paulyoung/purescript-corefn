module CoreFn.FromJSON where
  -- ( moduleFromJSON
  -- )

import Prelude

import Control.Alt ((<|>))
import CoreFn.Ann (Ann(..), Comment(..), SourcePos(..), SourceSpan(..))
import CoreFn.Expr (Bind(..), Bind', Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Meta (ConstructorType(..), Meta(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Array as Array
import Data.Foreign (F, Foreign, ForeignError(ForeignError, TypeMismatch), fail, readArray, readInt, readNull, readString, typeOf)
import Data.Foreign.Index (index, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.Keys (keys)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

objectType :: String
objectType = "object"

object :: forall a. (Foreign -> F a) -> Foreign -> F a
object _ json
  | typ <- typeOf json, typ /= objectType = fail $ TypeMismatch objectType typ
object f json = f json

constructorTypeFromJSON :: Foreign -> F ConstructorType
constructorTypeFromJSON json = do
  type_ <- readString json
  case type_ of
    "ProductType" -> pure ProductType
    "SumType" -> pure SumType
    _ -> fail $ ForeignError $ "Unknown ConstructorType: " <> type_

metaFromJSON :: Foreign -> F Meta
metaFromJSON = object $ \json -> do
  type_ <- readProp "metaType" json >>= readString
  case type_ of
    "IsConstructor" -> isConstructorFromJSON json
    "IsNewType" -> pure IsNewtype
    "IsTypeClassConstructor" -> pure IsTypeClassConstructor
    "IsForeign" -> pure IsForeign
    _ -> fail $ ForeignError $ "Unknown Meta type :" <> type_
  where
  isConstructorFromJSON :: Foreign -> F Meta
  isConstructorFromJSON json = do
    ct <- readProp "constructorType" json >>= constructorTypeFromJSON
    is <- readProp "identifiers" json >>= readArray >>= traverse identFromJSON
    pure $ IsConstructor ct is

annFromJSON :: FilePath -> Foreign -> F Ann
annFromJSON modulePath = object \json -> do
  sourceSpan <- readProp "sourceSpan" json >>= sourceSpanFromJSON
  meta <- readProp "meta" json >>= readNull >>= traverse metaFromJSON
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

qualifiedFromJSON :: forall a. (String -> a) -> Foreign -> F (Qualified a)
qualifiedFromJSON f = object \json -> do
  mn <- readProp "moduleName" json >>= readNull >>= traverse moduleNameFromJSON
  i <- readProp "identifier" json >>= map f <<< readString
  pure $ Qualified mn i

moduleNameFromJSON :: Foreign -> F ModuleName
moduleNameFromJSON json = map ModuleName $ readArray json
  >>= traverse properNameFromJSON

moduleFromJSON :: String -> F { version :: Version, module :: Module Ann }
moduleFromJSON = parseJSON >=> moduleFromJSON'
  where
  moduleFromJSON' :: Foreign -> F { version :: Version, module :: Module Ann }
  moduleFromJSON' = object \json -> do
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
    "NonRec" -> NonRec <$> bindFromJSON' json
    "Rec" ->
      map Rec
        $ readProp "binds" json
        >>= readArray
        >>= traverse (object bindFromJSON')
    _ -> fail $ ForeignError $ "Unknown Bind type: " <> type_
  where
  bindFromJSON' :: Foreign -> F (Bind' Ann)
  bindFromJSON' json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    ident <- readProp "identifier" json >>= identFromJSON
    -- expr <- readProp "expression" >>= exprFromJSON modulePath
    expr <- fail $ ForeignError "FIXME"
    pure $ Tuple (Tuple ann ident) expr

-- recordFromJSON

exprFromJSON :: FilePath -> Foreign -> F (Expr Ann)
exprFromJSON modulePath = object \json -> do
  type_ <- readProp "type" json >>= readString
  case type_ of
    "Var" -> varFromJSON json
    -- "Literal" -> literalFromJSON json
    -- "Constructor" -> constructorFromJSON json
    -- "Accessor" -> accessorFromJSON json
    -- "ObjectUpdate" -> objectUpdateFromJSON json
    -- "Abs" -> absFromJSON json
    -- "App" -> appFromJSON json
    -- "Case" -> caseFromJSON json
    -- "Let" -> letFromJSON json
    _ -> fail $ ForeignError $ "Unknown Expr type: " <> type_
  where
  varFromJSON :: Foreign -> F (Expr Ann)
  varFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    qi <- readProp "value" json >>= qualifiedFromJSON Ident
    pure $ Var ann qi
    
  -- literalFromJSON json = do
  -- constructorFromJSON json = do
  -- accessorFromJSON json = do
  -- objectUpdateFromJSON json = do
  -- absFromJSON json = do
  -- appFromJSON json = do
  -- caseFromJSON json = do
  -- letFromJSON json = do

-- caseAlternativeFromJSON

-- binderFromJSON
