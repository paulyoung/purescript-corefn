module CoreFn.Literal where

import Prelude

import Data.Either (Either, either)
import Data.Tuple (Tuple)

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
derive instance functorLiteral :: Functor Literal
derive instance ordLiteral :: Ord a => Ord (Literal a)

instance showLiteral :: Show a => Show (Literal a) where
  show (NumericLiteral e) = "(NumericLiteral " <> either show show e <> ")"
  show (StringLiteral s) = "(StringLiteral " <> show s <> ")"
  show (CharLiteral c) = "(CharLiteral " <> show c <> ")"
  show (BooleanLiteral b) = "(BooleanLiteral " <> show b <> ")"
  show (ArrayLiteral a) = "(ArrayLiteral " <> show a <> ")"
  show (ObjectLiteral o) = "(ObjectLiteral" <> show o <> ")"

-- readLiteral :: Foreign -> F (Literal (Expr Unit))
-- readLiteral x = do
--   label <- index x 0 >>= readString
--   readLiteral' label x

--   where

--   readValues :: Array Foreign -> F (Array (Expr Unit))
--   readValues = traverse readExpr

--   readPair :: Foreign -> String -> F (Tuple String (Expr Unit))
--   readPair obj key = Tuple key <$> (readProp key obj >>= readExpr)

--   readPairs :: Foreign -> Array String -> F (Array (Tuple String (Expr Unit)))
--   readPairs obj = sequence <<< (map <<< readPair) obj

--   readLiteral' :: String -> Foreign -> F (Literal (Expr Unit))
--   readLiteral' "IntLiteral" v = do
--     value <- index v 1
--     NumericLiteral <$> Left <$> readInt value
--   readLiteral' "NumberLiteral" v = do
--     value <- index v 1
--     NumericLiteral <$> Right <$> readNumber value
--   readLiteral' "StringLiteral" v = do
--     value <- index v 1
--     StringLiteral <$> readString value
--   readLiteral' "CharLiteral" v = do
--     value <- index v 1
--     CharLiteral <$> readChar value
--   readLiteral' "BooleanLiteral" v = do
--     value <- index v 1
--     BooleanLiteral <$> readBoolean value
--   readLiteral' "ArrayLiteral" v = do
--     array <- index v 1 >>= readArray
--     ArrayLiteral <$> readValues array
--   readLiteral' "ObjectLiteral" v = do
--     obj <- index v 1
--     keys <- K.keys obj
--     ObjectLiteral <$> readPairs obj keys
--   readLiteral' label _ = fail $ ForeignError $ "Unknown literal: " <> label

-- readLiteralJSON :: String -> F (Literal (Expr Unit))
-- readLiteralJSON = parseJSON >=> readLiteral
