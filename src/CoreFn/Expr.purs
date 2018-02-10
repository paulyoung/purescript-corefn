-- |
-- The core functional representation
--
module CoreFn.Expr
  ( Bind(..)
  , Bind'
  , CaseAlternative(..)
  , Expr(..)
  , Guard
  -- , readBind
  -- , readBindJSON
  -- , readExpr
  -- , readExprJSON
  -- , readLiteral
  -- , readLiteralJSON
  ) where

import Prelude

import CoreFn.Binders (Binder)
import CoreFn.Ident (Ident)
import CoreFn.Literal (Literal)
import CoreFn.Names (ProperName, Qualified)
import Data.Either (Either(..), either)
import Data.Profunctor.Strong ((***))
import Data.Traversable (intercalate)
import Data.Tuple (Tuple)

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- A data constructor (type name, constructor name, field names)
  --
  | Constructor a ProperName ProperName (Array Ident)
  -- |
  -- A record property accessor
  --
  | Accessor a String (Expr a) -- PSString
  -- |
  -- Partial record update
  --
  | ObjectUpdate a (Expr a) (Array (Tuple String (Expr a))) -- PSString
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
  -- |
  -- A case expression
  --
  | Case a (Array (Expr a)) (Array (CaseAlternative a))
  -- |
  -- A let binding
  --
  | Let a (Array (Bind a)) (Expr a)

derive instance eqExpr :: Eq a => Eq (Expr a)
-- derive instance functorExpr :: Functor Expr
derive instance ordExpr :: Ord a => Ord (Expr a)

instance showExpr :: Show a => Show (Expr a) where
  show (Literal a l) =
    "(Literal " <>
      intercalate " " [ show a, show l ] <>
    ")"
  show (Constructor a t c fs) =
    "(Constructor " <>
      intercalate " " [ show a, show t, show c, show fs] <>
    ")"
  show (Accessor a s e) =
    "(Accessor " <>
      intercalate " " [ show a, show s, show e ] <>
    ")"
  show (ObjectUpdate a e fs) =
    "(ObjectUpdate " <>
      intercalate " " [ show a, show e, show fs ] <>
    ")"
  show (Abs a i e) =
    "(Abs " <>
      intercalate " " [ show a, show i, show e ] <>
    ")"
  show (App a e1 e2) =
    "(App " <>
      intercalate " " [ show a, show e1, show e2 ] <>
    ")"
  show (Var a q) =
    "(Var " <>
      intercalate " " [ show a, show q ] <>
    ")"
  show (Case a es cs) =
    "(Case " <>
      intercalate " " [ show a, show es, show cs ] <>
    ")"
  show (Let a bs e) =
    "(Let " <>
      intercalate " " [ show a, show bs, show e ] <>
    ")"

-- readExpr :: Foreign -> F (Expr Unit)
-- readExpr x = do
--   label <- index x 0 >>= readString
--   readExpr' label x

--   where

--   readExpr' :: String -> Foreign -> F (Expr Unit)
--   readExpr' "Literal" y = do
--     value <- index y 1
--     Literal unit <$> readLiteral value
--   readExpr' "Abs" y = do
--     ident <- index y 1
--     expr <- index y 2
--     Abs unit <$> readIdent ident <*> readExpr expr
--   readExpr' "App" y = do
--     expr1 <- index y 1
--     expr2 <- index y 2
--     App unit <$> readExpr expr1 <*> readExpr expr2
--   readExpr' "Var" y = do
--     value <- index y 1
--     Var unit <$> readQualified Ident value
--   readExpr' label _ = fail $ ForeignError $ "Unknown expression: " <> label

-- readExprJSON :: String -> F (Expr Unit)
-- readExprJSON = parseJSON >=> readExpr

-- |
--  A let or module binding.
--
data Bind a
  = NonRec (Bind' a)
  | Rec (Array (Bind' a))

type Bind' a = Tuple (Tuple a Ident) (Expr a)

derive instance eqBind :: Eq a => Eq (Bind a)
-- derive instance functorBind :: Functor Bind
derive instance ordBind :: Ord a => Ord (Bind a)

instance showBind :: Show a => Show (Bind a) where
  show (NonRec b) = "(NonRec " <> show b <> ")"
  show (Rec b) = "(Rec " <> show b <> ")"

-- readBind :: Foreign -> F (Bind Unit)
-- readBind x = do
--   pairs <- objectProps x
--   bindings <- traverse fromPair pairs
--   pure $ Bind bindings

--   where

--   fromPair
--     :: { key :: String, value :: Foreign }
--     -> F (Tuple (Tuple Unit Ident) (Expr Unit))
--   fromPair pair = do
--     expr <- readExpr pair.value
--     let ident = Ident pair.key
--     pure $ Tuple (Tuple unit ident) expr

-- readBindJSON :: String -> F (Bind Unit)
-- readBindJSON = parseJSON >=> readBind


-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard a = Expr a


-- |
-- An alternative in a case statement
--
data CaseAlternative a = CaseAlternative
  -- |
  -- A collection of binders with which to match the inputs
  (Array (Binder a))
  -- |
  -- The result expression or a collect of guarded expressions
  (Either (Array (Tuple (Guard a) (Expr a))) (Expr a))

derive instance eqCaseAlternative :: Eq a => Eq (CaseAlternative a)
derive instance ordCaseAlternative :: Ord a => Ord (CaseAlternative a)

-- instance functorCaseAlternative :: Functor CaseAlternative where
--   map f (CaseAlternative cabs car) = CaseAlternative
--     (map (map f) cabs)
--     (either (Left <<< map (map f *** map f)) (Right <<< map f) car)

instance showCaseAlternative :: Show a => Show (CaseAlternative a) where
  show (CaseAlternative cabs car) =
    "(CaseAlternative " <>
      intercalate " " [ show cabs, show car ] <>
    ")"
