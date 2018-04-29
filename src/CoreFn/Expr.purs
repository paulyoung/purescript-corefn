-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , Binder(..)
  , CaseAlternative(..)
  , Expr(..)
  , Literal(..)
  , readBind
  , readBindJSON
  , readBinder
  , readBinderJSON
  , readCaseAlternative
  , readCaseAlternativeJSON
  , readExpr
  , readExprJSON
  , readLiteral
  , readLiteralJSON
  ) where

import Prelude

import Control.Alt ((<|>))
import CoreFn.Ident (Ident(..), readIdent)
import CoreFn.Names (ProperName(..), Qualified, readProperName, readQualified)
import CoreFn.Util (objectProps)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, ForeignError(..), fail, readArray, readBoolean, readChar, readInt, readNumber, readString)
import Data.Foreign.Index (errorAt, index, readProp)
import Data.Foreign.JSON (parseJSON)
import Data.Foreign.Keys as K
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Int Number)
  -- |
  -- A string literal
  --
  | StringLiteral String
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Boolean
  -- |
  -- An array literal
  --
  | ArrayLiteral (Array a)
  -- |
  -- An object literal
  --
  | ObjectLiteral (Array (Tuple String a))

derive instance eqLiteral :: Eq a => Eq (Literal a)
derive instance ordLiteral :: Ord a => Ord (Literal a)

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral e) = "(NumericLiteral " <> either show show e <> ")"
  show (StringLiteral s) = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c) = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b) = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral a) = "(ArrayLiteral " <> show a <> ")"
  show (ObjectLiteral o) = "(ObjectLiteral" <> show o <> ")"

readLiteral :: Foreign -> F (Literal (Expr Unit))
readLiteral = readLiteral' readExpr

readLiteral' :: forall a. (Foreign -> F a) -> Foreign -> F (Literal a)
readLiteral' f x = do
  label <- index x 0 >>= readString
  readLiteral'' label x

  where

  readValues :: Array Foreign -> F (Array a)
  readValues = traverse f

  readPair :: Foreign -> String -> F (Tuple String a)
  readPair obj key = Tuple key <$> (readProp key obj >>= f)

  readPairs :: Foreign -> Array String -> F (Array (Tuple String a))
  readPairs obj = sequence <<< (map <<< readPair) obj

  readLiteral'' :: String -> Foreign -> F (Literal a)
  readLiteral'' "IntLiteral" v = do
    value <- index v 1
    NumericLiteral <$> Left <$> readInt value
  readLiteral'' "NumberLiteral" v = do
    value <- index v 1
    NumericLiteral <$> Right <$> readNumber value
  readLiteral'' "StringLiteral" v = do
    value <- index v 1
    StringLiteral <$> readString value
  readLiteral'' "CharLiteral" v = do
    value <- index v 1
    CharLiteral <$> readChar value
  readLiteral'' "BooleanLiteral" v = do
    value <- index v 1
    BooleanLiteral <$> readBoolean value
  readLiteral'' "ArrayLiteral" v = do
    array <- index v 1 >>= readArray
    ArrayLiteral <$> readValues array
  readLiteral'' "ObjectLiteral" v = do
    obj <- index v 1
    keys <- K.keys obj
    ObjectLiteral <$> readPairs obj keys
  readLiteral'' label _ = fail $ ForeignError $ "Unknown literal: " <> label

readLiteralJSON :: String -> F (Literal (Expr Unit))
readLiteralJSON = parseJSON >=> readLiteral

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- Function introduction
  --
  | Abs a Ident (Expr a)
  -- |
  -- Function application
  --
  | App a (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)

instance showExpr :: Show a => Show (Expr a) where
  show (Literal x y) = "(Literal " <> show x <> " " <> show y <> ")"
  show (Abs x y z) = "(Abs " <> show x <> " " <> show y <> " " <> show z <> ")"
  show (App x y z) = "(App " <> show x <> " " <> show y <> " " <> show z <> ")"
  show (Var x y) = "(Var " <> show x <> " " <> show y <> ")"

readExpr :: Foreign -> F (Expr Unit)
readExpr x = do
  label <- index x 0 >>= readString
  readExpr' label x

  where

  readExpr' :: String -> Foreign -> F (Expr Unit)
  readExpr' "Literal" y = do
    value <- index y 1
    Literal unit <$> readLiteral value
  readExpr' "Abs" y = do
    ident <- index y 1
    expr <- index y 2
    Abs unit <$> readIdent ident <*> readExpr expr
  readExpr' "App" y = do
    expr1 <- index y 1
    expr2 <- index y 2
    App unit <$> readExpr expr1 <*> readExpr expr2
  readExpr' "Var" y = do
    value <- index y 1
    Var unit <$> readQualified Ident value
  readExpr' label _ = fail $ ForeignError $ "Unknown expression: " <> label

readExprJSON :: String -> F (Expr Unit)
readExprJSON = parseJSON >=> readExpr

-- |
--  A let or module binding.
--
data Bind a = Bind (Array (Tuple (Tuple a Ident) (Expr a)))

derive instance eqBind :: Eq a => Eq (Bind a)
derive instance ordBind :: Ord a => Ord (Bind a)

instance showBind :: Show a => Show (Bind a) where
  show (Bind x) = "(Bind " <> show x <> ")"

readBind :: Foreign -> F (Bind Unit)
readBind x = do
  pairs <- objectProps x
  bindings <- traverse fromPair pairs
  pure $ Bind bindings

  where

  fromPair
    :: { key :: String, value :: Foreign }
    -> F (Tuple (Tuple Unit Ident) (Expr Unit))
  fromPair pair = do
    expr <- readExpr pair.value
    let ident = Ident pair.key
    pure $ Tuple (Tuple unit ident) expr

readBindJSON :: String -> F (Bind Unit)
readBindJSON = parseJSON >=> readBind

newtype CaseAlternative a = CaseAlternative
  { binders :: Array (Binder a)
  , result :: Either (Array (Tuple (Expr a) (Expr a))) (Expr a)
  }

derive instance eqCaseAlternative :: Eq a => Eq (CaseAlternative a)
derive instance ordCaseAlternative :: Ord a => Ord (CaseAlternative a)
derive instance genericCaseAlternative :: Generic (CaseAlternative a) _

instance showCaseAlternative :: Show a => Show (CaseAlternative a) where
  show = genericShow

readCaseAlternative :: Foreign -> F (CaseAlternative Unit)
readCaseAlternative x = do
  binders <- index x 0 >>= readArray
  result <- index x 1
  record <- {binders: _, result: _} <$> traverse readBinder binders <*> readResult result
  pure (CaseAlternative record)

  where

  readResult :: Foreign -> F (Either (Array (Tuple (Expr Unit) (Expr Unit))) (Expr Unit))
  readResult y = map Left (readGuardedExprs y) <|> map Right (readExpr y)

  readGuardedExprs :: Foreign -> F (Array (Tuple (Expr Unit) (Expr Unit)))
  readGuardedExprs y = do
    guardedExprs <- readArray y
    traverse readGuardedExpr guardedExprs

  readGuardedExpr :: Foreign -> F (Tuple (Expr Unit) (Expr Unit))
  readGuardedExpr y = do
    guard <- index y 1
    expr <- index y 2
    Tuple <$> readExpr guard <*> readExpr expr

readCaseAlternativeJSON :: String -> F (CaseAlternative Unit)
readCaseAlternativeJSON = parseJSON >=> readCaseAlternative

data Binder a
  = NullBinder a
  | LiteralBinder a (Literal (Binder a))
  | VarBinder a Ident
  | ConstructorBinder a (Qualified ProperName) (Qualified ProperName) (Array (Binder a))
  | NamedBinder a Ident (Binder a)

derive instance eqBinder :: Eq a => Eq (Binder a)
derive instance ordBinder :: Ord a => Ord (Binder a)
derive instance genericBinder :: Generic (Binder a) _

instance showBinder :: Show a => Show (Binder a) where
  show x = genericShow x

readBinder :: Foreign -> F (Binder Unit)
readBinder x = nullBinder <|> notNullBinder

  where

  notNullBinder = do
    label <- index x 0 >>= readString
    readBinder' label

  nullBinder = do
    binder <- readString x
    case binder of
      "NullBinder" -> pure (NullBinder unit)
      _ -> fail $ ForeignError $ "Not NullBinder: " <> binder

  readBinder' :: String -> F (Binder Unit)
  readBinder' "LiteralBinder" = do
    literal <- index x 1
    LiteralBinder unit <$> readLiteral' readBinder literal
  readBinder' "VarBinder" = do
    ident <- index x 1
    VarBinder unit <$> readIdent ident
  readBinder' "ConstructorBinder" = do
    moduleName <- index x 1
    name <- index x 2
    binders <- index x 3 >>= readArray
    ConstructorBinder unit <$> readQualified ProperName moduleName <*> readQualified ProperName name <*> traverse readBinder binders
  readBinder' "NamedBinder" = do
    ident <- index x 1
    binder <- index x 2
    NamedBinder unit <$> readIdent ident <*> readBinder binder
  readBinder' label =
    fail $ errorAt 0 $ ForeignError $ "Unknown binder: " <> label

readBinderJSON :: String -> F (Binder Unit)
readBinderJSON = parseJSON >=> readBinder
