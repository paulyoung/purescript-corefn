module CoreFn.Ann where

import Prelude

import CoreFn.Meta (Meta)
import CoreFn.Names (OpName, ProperName, Qualified)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- |
-- Type alias for basic annotations
--
newtype Ann = Ann
  { sourceSpan :: SourceSpan
  , comments :: Array Comment
  , type :: Maybe Type
  , meta :: Maybe Meta
  }

derive instance newtypeAnn :: Newtype Ann _
derive instance eqAnn :: Eq Ann
derive instance ordAnn :: Ord Ann

instance showAnn :: Show Ann where
  show (Ann ann) =
    "(Ann " <>
      "{ sourceSpan: " <> show ann.sourceSpan <>
      ", comments: " <> show ann.comments <>
      ", type: " <> show ann.type <>
      ", meta: " <> show ann.meta <>
      "}" <>
    ")"

-- |
-- An annotation empty of metadata aside from a source span.
--
ssAnn :: SourceSpan -> Ann
ssAnn = Ann <<<
  { sourceSpan: _
  , comments: []
  , type: Nothing
  , meta: Nothing
  }

-- |
-- Remove the comments from an annotation
--
removeComments :: Ann -> Ann
removeComments (Ann ann) = Ann $ ann { comments = [] }


-- |
-- Defines the types of source code comments
--
data Comment
  = LineComment String
  | BlockComment String

derive instance eqComment :: Eq Comment
derive instance ordComment :: Ord Comment

instance showComment :: Show Comment where
  show (LineComment s) = "(LineComment " <> show s <> ")"
  show (BlockComment s) = "(BlockComment " <> show s <> ")"


-- | Source position information
data SourcePos = SourcePos
  { sourcePosLine :: Int
    -- ^ Line number
  , sourcePosColumn :: Int
    -- ^ Column number
  }

derive instance eqSourcePos :: Eq SourcePos
derive instance ordSourcePos :: Ord SourcePos

instance showSourcePos :: Show SourcePos where
 show (SourcePos { sourcePosLine, sourcePosColumn }) =
   "(SourcePos " <>
     "{ sourcePosLine: " <> show sourcePosLine <>
     ", sourcePosColumn: " <> show sourcePosColumn <>
     "}" <>
   ")"


data SourceSpan = SourceSpan
  { spanName :: String
    -- ^ Source name
  , spanStart :: SourcePos
    -- ^ Start of the span
  , spanEnd :: SourcePos
    -- ^ End of the span
  }

derive instance eqSourceSpan :: Eq SourceSpan
derive instance ordSourceSpan :: Ord SourceSpan

instance showSourceSpan :: Show SourceSpan where
  show (SourceSpan { spanName, spanStart, spanEnd }) =
    "(SourceSpan " <>
      "{ spanName: " <> show spanName <>
      ", spanStart: " <> show spanStart <>
      ", spanEnd: " <> show spanEnd <>
      "}" <>
    ")"


-- |
-- The type of types
--
data Type
  -- | A unification variable of type Type
  = TUnknown Int
  -- | A named type variable
  | TypeVar String
  -- | A type-level string
  | TypeLevelString String -- PSString
  -- | A type wildcard, as would appear in a partial type synonym
  | TypeWildcard SourceSpan
  -- | A type constructor
  | TypeConstructor (Qualified ProperName)
  -- | A type operator. This will be desugared into a type constructor during the
  -- "operators" phase of desugaring.
  | TypeOp (Qualified OpName)
  -- | A type application
  | TypeApp Type Type
  -- | Forall quantifier
  | ForAll String Type (Maybe SkolemScope)
  -- | A type with a set of type class constraints
  | ConstrainedType Constraint Type
  -- | A skolem constant
  | Skolem String Int SkolemScope (Maybe SourceSpan)
  -- | An empty row
  | REmpty
  -- | A non-empty row
  | RCons Label Type Type
  -- | A type with a kind annotation
  | KindedType Type Kind
  -- | A placeholder used in pretty printing
  | PrettyPrintFunction Type Type
  -- | A placeholder used in pretty printing
  | PrettyPrintObject Type
  -- | A placeholder used in pretty printing
  | PrettyPrintForAll (Array String) Type
  -- | Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  | BinaryNoParensType Type Type Type
  -- | Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  | ParensInType Type

derive instance eqType :: Eq Type
derive instance ordType :: Ord Type

instance showType :: Show Type where
  show (TUnknown i) = "(TUnknown " <> show i <> ")"
  show (TypeVar s) = "(TypeVar " <> show s <> ")"
  show (TypeLevelString s) = "(TypeLevelString " <> show s <> ")"
  show (TypeWildcard so) = "(TypeWildcard " <> show so <> ")"
  show (TypeConstructor q) = "(TypeConstructor " <> show q <> ")"
  show (TypeOp q) = "(TypeOp " <> show q <> ")"
  show (TypeApp t1 t2) = "(TypeApp " <> show t1 <> " " <> show t2 <> ")"
  show (ForAll s t sk) =
    Array.intercalate " "
      [ "(Forall", show s, show t, show sk, ")"]
  show (ConstrainedType c t) =
    "(ConstrainedType " <> show c <> " " <> show t <> ")"
  show (Skolem s i sk ss) =
    Array.intercalate " " [ "(Skolem", show s, show i, show sk, show ss, ")" ]
  show REmpty = "Rempty"
  show (RCons l t1 t2) =
    Array.intercalate " " [ "(RCons", show l, show t1, show t2, ")" ]
  show (KindedType t k) = "(KindedType " <> show t <> show k <> ")"
  show (PrettyPrintFunction t1 t2) =
    "(PrettyPrintFunction " <> show t1 <> show t2 <> show ")"
  show (PrettyPrintObject t) = "(PrettyPrintObject " <> show t <> ")"
  show (PrettyPrintForAll ss t) =
    "(PrettyPrintForAll " <> show ss <> show t <> ")"
  show (BinaryNoParensType t1 t2 t3) =
    Array.intercalate " "
      [ "(BinaryNoParensType", show t1, show t2, show t3, ")" ]
  show (ParensInType t) = "(ParensInType " <> show t <> ")"

-- | The data type of kinds
data Kind
  -- | Unification variable of type Kind
  = KUnknown Int
  -- | Kinds for labelled, unordered rows without duplicates
  | Row Kind
  -- | Function kinds
  | FunKind Kind Kind
  -- | A named kind
  | NamedKind (Qualified ProperName)

derive instance eqKind :: Eq Kind
derive instance ordKind :: Ord Kind

instance showKind :: Show Kind where
  show (KUnknown i) = "(KUnknown " <> show i <> ")"
  show (Row k) = "(Row " <> show k <> ")"
  show (FunKind k1 k2) = "(FunKind " <> show k1 <> show k2 <> ")"
  show (NamedKind q) = "(NamedKind " <> show q <> ")"


-- |
-- An identifier for the scope of a skolem variable
--
newtype SkolemScope = SkolemScope Int

derive newtype instance eqSkolemScope :: Eq SkolemScope
derive newtype instance ordSkolemScope :: Ord SkolemScope

instance showSkolemScope :: Show SkolemScope where
  show s = "(SkolemScope " <> show s <> ")"


-- | Additional data relevant to type class constraints
data ConstraintData
  = PartialConstraintData (Array (Array String)) Boolean
  -- ^ Data to accompany a Partial constraint generated by the exhaustivity checker.
  -- It contains (rendered) binder information for those binders which were
  -- not matched, and a flag indicating whether the list was truncated or not.
  -- Note: we use 'String' here because using 'Binder' would introduce a cyclic
  -- dependency in the module graph.

derive instance eqConstraintData :: Eq ConstraintData
derive instance ordConstraintData :: Ord ConstraintData

instance showConstraintData :: Show ConstraintData where
  show (PartialConstraintData bs t)
    = "(PartialConstraintData " <> show bs <> " " <> show t <> ")"


-- | A typeclass constraint
data Constraint = Constraint
  { constraintClass :: Qualified ProperName
  -- ^ constraint class name
  , constraintArgs :: Array Type
  -- ^ type arguments
  , constraintData :: Maybe ConstraintData
  -- ^ additional data relevant to this constraint
  }

derive instance eqConstraint :: Eq Constraint
derive instance ordConstraint :: Ord Constraint

instance showConstraint :: Show Constraint where
  show (Constraint { constraintClass, constraintArgs, constraintData }) =
    "(Constraint " <>
      "{ constraintClass: " <> show constraintClass <>
      ", constraintArgs: " <> show constraintArgs <>
      ", constraintData: " <> show constraintData <>
      "}" <>
    ")"


-- |
-- Labels are used as record keys and row entry names. Labels newtype PSString
-- because records are indexable by PureScript strings at runtime.
--
newtype Label = Label String -- PSString

derive newtype instance eqLabel :: Eq Label
derive newtype instance ordLabel :: Ord Label

instance showLabel :: Show Label where
  show l = "(Label " <> show l <> ")"
