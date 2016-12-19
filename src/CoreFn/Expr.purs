-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Expr(..)
  , Literal(..)
  , readExpr
  , readExprJSON
  , readLiteral
  , readLiteralJSON
  ) where

import Prelude
import Data.Foreign.Keys as K
import CoreFn.Ident (Ident(..))
import CoreFn.Names (Qualified, readQualified)
import CoreFn.Util (foreignError)
import Data.Either (Either(..), either)
import Data.Foreign (F, Foreign, parseJSON, readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign)
import Data.Foreign.Class (readProp)
import Data.Foreign.Index (prop)
import Data.Generic (class Generic)
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
derive instance genericLiteral :: Generic a => Generic (Literal a)
derive instance ordLiteral :: Ord a => Ord (Literal a)

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral e) = "(NumericLiteral " <> either show show e <> ")"
  show (StringLiteral s) = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c) = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b) = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral a) = "(ArrayLiteral " <> show a <> ")"
  show (ObjectLiteral o) = "(ObjectLiteral" <> show o <> ")"

readLiteral :: Foreign -> F (Literal (Expr Foreign))
readLiteral x = do
  label <- readProp 0 x >>= readString
  readLiteral' label x

  where

  readValues :: Array Foreign -> F (Array (Expr Foreign))
  readValues = traverse readExpr

  readPair :: Foreign -> String -> F (Tuple String (Expr Foreign))
  readPair obj key = Tuple key <$> (prop key obj >>= readExpr)

  readPairs :: Foreign -> Array String -> F (Array (Tuple String (Expr Foreign)))
  readPairs obj = sequence <<< (map <<< readPair) obj

  readLiteral' :: String -> Foreign -> F (Literal (Expr Foreign))
  readLiteral' "IntLiteral" v = do
    value <- readProp 1 v
    NumericLiteral <$> Left <$> readInt value
  readLiteral' "NumberLiteral" v = do
    value <- readProp 1 v
    NumericLiteral <$> Right <$> readNumber value
  readLiteral' "StringLiteral" v = do
    value <- readProp 1 v
    StringLiteral <$> readString value
  readLiteral' "CharLiteral" v = do
    value <- readProp 1 v
    CharLiteral <$> readChar value
  readLiteral' "BooleanLiteral" v = do
    value <- readProp 1 v
    BooleanLiteral <$> readBoolean value
  readLiteral' "ArrayLiteral" v = do
    array <- readProp 1 v >>= readArray
    ArrayLiteral <$> readValues array
  readLiteral' "ObjectLiteral" v = do
    obj <- readProp 1 v
    keys <- K.keys obj
    ObjectLiteral <$> readPairs obj keys
  readLiteral' label _ = foreignError $ "Unknown literal: " <> label

readLiteralJSON :: String -> F (Literal (Expr Foreign))
readLiteralJSON json = parseJSON json >>= readLiteral

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- Function application
  --
  | App (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)

derive instance genericExpr :: Generic a => Generic (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)

instance eqExpr :: Eq (Expr a) where
  eq (Literal _ l1) (Literal _ l2) = l1 == l2
  eq (App x1 y1) (App x2 y2) = x1 == x2 && y1 == y2
  eq (Var _ v1) (Var _ v2) = v1 == v2
  eq _ _ = false

instance showExpr :: Show (Expr a) where
  show (Literal _ l) = "(Literal " <> "_" <> " " <> show l <> ")"
  show (App e1 e2) = "(App" <> show e1 <> " " <> show e2 <> ")"
  show (Var _ v) = "(Var " <> "_" <> " " <> show v <> ")"

readExpr :: Foreign -> F (Expr Foreign)
readExpr x = do
  label <- readProp 0 x >>= readString
  readExpr' label x

  where

  readExpr' :: String -> Foreign -> F (Expr Foreign)
  readExpr' "Literal" y = do
    value <- readProp 1 y
    Literal (toForeign unit) <$> readLiteral value
  readExpr' "App" y = do
    expr1 <- readProp 1 y
    expr2 <- readProp 2 y
    App <$> readExpr expr1 <*> readExpr expr2
  readExpr' "Var" y = do
    value <- readProp 1 y
    Var (toForeign unit) <$> readQualified Ident value
  readExpr' label _ = foreignError $ "Unknown expression: " <> label

readExprJSON :: String -> F (Expr Foreign)
readExprJSON json = parseJSON json >>= readExpr
