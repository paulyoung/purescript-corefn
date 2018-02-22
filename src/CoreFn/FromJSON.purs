module CoreFn.FromJSON where
  -- ( moduleFromJSON
  -- )

import Prelude

import Control.Alt ((<|>))
import CoreFn.Ann (Ann(..), Comment(..), SourcePos(..), SourceSpan(..))
import CoreFn.Binders (Binder(..))
import CoreFn.Expr (Bind(..), Bind', CaseAlternative(..), Expr(..))
import CoreFn.Ident (Ident(..))
import CoreFn.Literal (Literal(..))
import CoreFn.Meta (ConstructorType(..), Meta(..))
import CoreFn.Module (FilePath(..), Module(..), ModuleImport(..), Version(..))
import CoreFn.Names (ModuleName(..), ProperName(..), Qualified(..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readChar, readInt, readNull, readNumber, readString, typeOf)
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

literalFromJSON :: forall a. (Foreign -> F a) -> Foreign -> F (Literal a)
literalFromJSON t = object \json -> do
  type_ <- readProp "literalType" json >>= readString
  case type_ of
    "IntLiteral" ->
      NumericLiteral <<< Left <$> (readProp "value" json >>= readInt)
    "NumberLiteral" ->
      NumericLiteral <<< Right <$> (readProp "value" json >>= readNumber)
    "StringLiteral" ->
      StringLiteral <$> (readProp "value" json >>= readString)
    "CharLiteral" ->
      CharLiteral <$> (readProp "value" json >>= readChar)
    "BooleanLiteral" ->
      BooleanLiteral <$> (readProp "value" json >>= readBoolean)
    "ArrayLiteral" -> parseArrayLiteral json
    "ObjectLiteral" -> parseObjectLiteral json
    _ -> fail $ ForeignError $ "Unknown Literal: " <> type_
  where
    parseArrayLiteral :: Foreign -> F (Literal a)
    parseArrayLiteral json = do
      val <- readProp "value" json >>= readArray
      as <- traverse t val
      pure $ ArrayLiteral as

    parseObjectLiteral :: Foreign -> F (Literal a)
    parseObjectLiteral json = do
      val <- readProp "value" json
      ObjectLiteral <$> recordFromJSON t val

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
    expr <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Tuple (Tuple ann ident) expr

recordFromJSON
  :: forall a
   . (Foreign -> F a)
  -> Foreign
  -> F (Array (Tuple String a))
recordFromJSON f json = keys json >>= traverse \key -> do
  value <- readProp key json >>= f
  pure $ Tuple key value

exprFromJSON :: FilePath -> Foreign -> F (Expr Ann)
exprFromJSON modulePath = object \json -> do
  type_ <- readProp "type" json >>= readString
  case type_ of
    "Var" -> varFromJSON json
    "Literal" -> literalExprFromJSON json
    "Constructor" -> constructorFromJSON json
    "Accessor" -> accessorFromJSON json
    "ObjectUpdate" -> objectUpdateFromJSON json
    "Abs" -> absFromJSON json
    "App" -> appFromJSON json
    "Case" -> caseFromJSON json
    "Let" -> letFromJSON json
    _ -> fail $ ForeignError $ "Unknown Expr type: " <> type_
  where
  varFromJSON :: Foreign -> F (Expr Ann)
  varFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    qi <- readProp "value" json >>= qualifiedFromJSON Ident
    pure $ Var ann qi

  literalExprFromJSON :: Foreign -> F (Expr Ann)
  literalExprFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    lit <- readProp "value" json >>= literalFromJSON (exprFromJSON modulePath)
    pure $ Literal ann lit

  constructorFromJSON :: Foreign -> F (Expr Ann)
  constructorFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    tyn <- readProp "typeName" json >>= properNameFromJSON
    con <- readProp "constructorName" json >>= properNameFromJSON
    is  <- readProp "fieldNames" json >>= readArray >>= traverse identFromJSON
    pure $ Constructor ann tyn con is

  accessorFromJSON :: Foreign -> F (Expr Ann)
  accessorFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    f <- readProp "fieldName" json >>= readString
    e <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Accessor ann f e

  objectUpdateFromJSON :: Foreign -> F (Expr Ann)
  objectUpdateFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    e <- readProp "expression" json >>= exprFromJSON modulePath
    us <- readProp "updates" json >>= recordFromJSON (exprFromJSON modulePath)
    pure $ ObjectUpdate ann e us

  absFromJSON :: Foreign -> F (Expr Ann)
  absFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    idn <- readProp "argument" json >>= identFromJSON
    e <- readProp "body" json >>= exprFromJSON modulePath
    pure $ Abs ann idn e

  appFromJSON :: Foreign -> F (Expr Ann)
  appFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    e <- readProp "abstraction" json >>= exprFromJSON modulePath
    e' <- readProp "argument" json >>= exprFromJSON modulePath
    pure $ App ann e e'

  caseFromJSON :: Foreign -> F (Expr Ann)
  caseFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    cs <- readProp "caseExpressions" json
      >>= readArray
      >>= traverse (exprFromJSON modulePath)
    cas <- readProp "caseAlternatives" json
      >>= readArray
      >>= traverse (caseAlternativeFromJSON modulePath)
    pure $ Case ann cs cas

  letFromJSON :: Foreign -> F (Expr Ann)
  letFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    bs <- readProp "binds" json
      >>= readArray
      >>= traverse (bindFromJSON modulePath)
    e <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Let ann bs e

caseAlternativeFromJSON :: FilePath -> Foreign -> F (CaseAlternative Ann)
caseAlternativeFromJSON modulePath = object \json -> do
  bs <- readProp "binders" json
    >>= readArray
    >>= traverse (binderFromJSON modulePath)
  isGuarded <- readProp "isGuarded" json >>= readBoolean
  if isGuarded
    then do
      es <- readProp "expressions" json
        >>= readArray
        >>= traverse parseResultWithGuard
      pure $ CaseAlternative
        { caseAlternativeBinders: bs
        , caseAlternativeResult: Left es
        }
    else do
      e <- readProp "expression" json >>= exprFromJSON modulePath
      pure $ CaseAlternative
        { caseAlternativeBinders: bs
        , caseAlternativeResult: Right e
        }
  where
  parseResultWithGuard :: Foreign -> F (Tuple (Expr Ann) (Expr Ann))
  parseResultWithGuard = object \json -> do
    g <- readProp "guard" json >>= exprFromJSON modulePath
    e <- readProp "expression" json >>= exprFromJSON modulePath
    pure $ Tuple g e

binderFromJSON :: FilePath -> Foreign -> F (Binder Ann)
binderFromJSON modulePath = object \json -> do
  type_ <- readProp "binderType" json >>= readString
  case type_ of
    "NullBinder" -> nullBinderFromJSON json
    "VarBinder" -> varBinderFromJSON json
    "LiteralBinder" -> literalBinderFromJSON json
    "ConstructorBinder" -> constructorBinderFromJSON json
    "NamedBinder" -> namedBinderFromJSON json
    _ -> fail $ ForeignError $ "Unknown Binder type: " <> type_
  where
  nullBinderFromJSON :: Foreign -> F (Binder Ann)
  nullBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    pure $ NullBinder ann

  varBinderFromJSON :: Foreign -> F (Binder Ann)
  varBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    idn <- readProp "identifier" json >>= identFromJSON
    pure $ VarBinder ann idn

  literalBinderFromJSON :: Foreign -> F (Binder Ann)
  literalBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    lit <- readProp "literal" json
      >>= literalFromJSON (binderFromJSON modulePath)
    pure $ LiteralBinder ann lit

  constructorBinderFromJSON :: Foreign -> F (Binder Ann)
  constructorBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    tyn <- readProp "typeName" json >>= qualifiedFromJSON ProperName
    con <- readProp "constructorName" json >>= qualifiedFromJSON ProperName
    bs <- readProp "binders" json
      >>= readArray
      >>= traverse (binderFromJSON modulePath)
    pure $ ConstructorBinder ann tyn con bs

  namedBinderFromJSON :: Foreign -> F (Binder Ann)
  namedBinderFromJSON json = do
    ann <- readProp "annotation" json >>= annFromJSON modulePath
    n <- readProp "identifier" json >>= identFromJSON
    b <- readProp "binder" json >>= binderFromJSON modulePath
    pure $ NamedBinder ann n b
