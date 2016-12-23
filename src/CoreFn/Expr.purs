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
import Data.Foreign (F, Foreign, parseJSON, readArray, readBoolean, readChar, readInt, readNumber, readString)
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

readLiteral :: Foreign -> F (Literal (Expr Unit))
readLiteral x = do
  label <- readProp 0 x >>= readString
  readLiteral' label x

  where

  readValues :: Array Foreign -> F (Array (Expr Unit))
  readValues = traverse readExpr

  readPair :: Foreign -> String -> F (Tuple String (Expr Unit))
  readPair obj key = Tuple key <$> (prop key obj >>= readExpr)

  readPairs :: Foreign -> Array String -> F (Array (Tuple String (Expr Unit)))
  readPairs obj = sequence <<< (map <<< readPair) obj

  readLiteral' :: String -> Foreign -> F (Literal (Expr Unit))
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

readLiteralJSON :: String -> F (Literal (Expr Unit))
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

derive instance eqExpr :: Eq a => Eq (Expr a)
derive instance genericExpr :: Generic a => Generic (Expr a)
derive instance ordExpr :: Ord a => Ord (Expr a)

instance showExpr :: Show a => Show (Expr a) where
  show (Literal x y) = "(Literal " <> show x <> " " <> show y <> ")"
  show (App x y) = "(App " <> show x <> " " <> show y <> ")"
  show (Var x y) = "(Var " <> show x <> " " <> show y <> ")"

readExpr :: Foreign -> F (Expr Unit)
readExpr x = do
  label <- readProp 0 x >>= readString
  readExpr' label x

  where

  readExpr' :: String -> Foreign -> F (Expr Unit)
  readExpr' "Literal" y = do
    value <- readProp 1 y
    Literal unit <$> readLiteral value
  readExpr' "App" y = do
    expr1 <- readProp 1 y
    expr2 <- readProp 2 y
    App <$> readExpr expr1 <*> readExpr expr2
  readExpr' "Var" y = do
    value <- readProp 1 y
    Var unit <$> readQualified Ident value
  readExpr' label _ = foreignError $ "Unknown expression: " <> label

readExprJSON :: String -> F (Expr Unit)
readExprJSON json = parseJSON json >>= readExpr
