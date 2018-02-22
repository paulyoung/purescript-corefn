module Test.CoreFn.FromJSON where

import Prelude

import Control.Monad.Except (runExcept)
import CoreFn.Ann (SourcePos(..), SourceSpan(..), ssAnn)
import CoreFn.FromJSON (moduleFromJSON)
import CoreFn.Ident (Ident(..))
import CoreFn.Module (FilePath(..), ModuleImport(..))
import CoreFn.Names (ModuleName(..), ProperName(..))
import Data.Either (isRight)
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec = describe "FromJSON" do
  let
    mn = ModuleName [ ProperName "Example", ProperName "Main" ]
    mp = FilePath "src/Example/Main.purs"
    ss = SourceSpan
      { spanName: unwrap mp
      , spanStart: SourcePos { sourcePosLine: 0, sourcePosColumn: 0 }
      , spanEnd: SourcePos { sourcePosLine: 0, sourcePosColumn: 0 }
      }
    ann = ssAnn ss

  it "should parse an empty module" do
    let m = runExcept $ moduleFromJSON """
      {"moduleName":["Example","Main"],"imports":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"moduleName":["Example","Main"]}],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[],"comments":[],"foreign":[]}
    """
    isRight m `shouldEqual` true
    traverse_ (_.module >>> unwrap >>> _.moduleName >>> shouldEqual mn) m

  it "should parse module path" do
    let m = runExcept $ moduleFromJSON """
      {"moduleName":["Example","Main"],"imports":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"moduleName":["Example","Main"]}],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[],"comments":[],"foreign":[]}
    """
    isRight m `shouldEqual` true
    traverse_ (_.module >>> unwrap >>> _.modulePath >>> shouldEqual mp) m

  it "should parse imports" do
    let m = runExcept $ moduleFromJSON """
      {"moduleName":["Example","Main"],"imports":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"moduleName":["Example","Main"]}],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[],"comments":[],"foreign":[]}
    """
    isRight m `shouldEqual` true
    traverse_ (_.module >>> unwrap >>> _.moduleImports >>> shouldEqual [ ModuleImport { ann, moduleName: mn } ]) m

  it "should parse exports" do
    let m = runExcept $ moduleFromJSON """
      {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":["exp"],"decls":[],"comments":[],"foreign":[]}
    """
    isRight m `shouldEqual` true
    traverse_ (_.module >>> unwrap >>> _.moduleExports >>> shouldEqual [ Ident "exp" ]) m

  it "should parse foreign" do
    let m = runExcept $ moduleFromJSON """
      {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[],"comments":[],"foreign":["exp"]}
    """
    isRight m `shouldEqual` true
    traverse_ (_.module >>> unwrap >>> _.moduleForeign >>> shouldEqual [ Ident "exp" ]) m

  describe "Expr" do
    it "should parse literals" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x1","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"IntLiteral","value":1},"type":"Literal"},"bindType":"NonRec"},{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x2","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"NumberLiteral","value":1},"type":"Literal"},"bindType":"NonRec"},{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x3","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"StringLiteral","value":"abc"},"type":"Literal"},"bindType":"NonRec"},{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x4","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"c"},"type":"Literal"},"bindType":"NonRec"},{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x5","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"BooleanLiteral","value":true},"type":"Literal"},"bindType":"NonRec"},{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x6","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"ArrayLiteral","value":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"}]},"type":"Literal"},"bindType":"NonRec"},{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x7","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"ObjectLiteral","value":[["a",{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"}]]},"type":"Literal"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse Constructor" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"constructor","expression":{"constructorName":"Left","annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"typeName":"Either","fieldNames":["value0"],"type":"Constructor"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse Accessor" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"ObjectLiteral","value":[["field",{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"IntLiteral","value":1},"type":"Literal"}]]},"type":"Literal"},"fieldName":"field","type":"Accessor"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse ObjectUpdate" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"objectUpdate","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"ObjectLiteral","value":[["field",{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"StringLiteral","value":"abc"},"type":"Literal"}]]},"type":"Literal"},"updates":[["field",{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"StringLiteral","value":"xyz"},"type":"Literal"}]],"type":"ObjectUpdate"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse Abs" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"abs","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"body":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":["Example","Main"],"identifier":"x"},"type":"Var"},"argument":"x","type":"Abs"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse App" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"app","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"argument":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"c"},"type":"Literal"},"type":"App","abstraction":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"body":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"},"argument":"x","type":"Abs"}},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse Case" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"case","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"caseExpressions":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"}],"caseAlternatives":[{"binders":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"binderType":"NullBinder"}],"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"isGuarded":false}],"type":"Case"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse Case with guards" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"case","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"caseExpressions":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"}],"caseAlternatives":[{"binders":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"binderType":"NullBinder"}],"expressions":[{"guard":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"BooleanLiteral","value":true},"type":"Literal"},"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"}}],"isGuarded":true}],"type":"Case"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse Let" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"case","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"binds":[{"binds":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"a","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"}}],"bindType":"Rec"}],"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"BooleanLiteral","value":true},"type":"Literal"},"type":"Let"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

  describe "Meta" do
    it "should parse IsConstructor" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":{"metaType":"IsConstructor","identifiers":["x"],"constructorType":"ProductType"},"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x","expression":{"annotation":{"meta":{"metaType":"IsConstructor","identifiers":[],"constructorType":"SumType"},"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse IsNewtype" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":{"metaType":"IsNewtype"},"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse IsTypeClassConstructor" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":{"metaType":"IsTypeClassConstructor"},"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse IsForeign" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":{"metaType":"IsForeign"},"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"x","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

  describe "Binders" do
    it "should parse LiteralBinder" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"case","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"caseExpressions":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"}],"caseAlternatives":[{"binders":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"literal":{"literalType":"BooleanLiteral","value":true},"binderType":"LiteralBinder"}],"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"isGuarded":false}],"type":"Case"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse VarBinder" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"case","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"caseExpressions":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"}],"caseAlternatives":[{"binders":[{"constructorName":{"moduleName":null,"identifier":"Left"},"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"typeName":{"moduleName":["Data","Either"],"identifier":"Either"},"binders":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"z","binderType":"VarBinder"}],"binderType":"ConstructorBinder"}],"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"isGuarded":false}],"type":"Case"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse NamedBinder" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"case","expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"caseExpressions":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"moduleName":null,"identifier":"x"},"type":"Var"}],"caseAlternatives":[{"binders":[{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"w","binder":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"w'","binder":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"identifier":"w''","binderType":"VarBinder"},"binderType":"NamedBinder"},"binderType":"NamedBinder"}],"expression":{"annotation":{"meta":null,"sourceSpan":{"start":[0,0],"end":[0,0]}},"value":{"literalType":"CharLiteral","value":"a"},"type":"Literal"},"isGuarded":false}],"type":"Case"},"bindType":"NonRec"}],"comments":[],"foreign":[]}
      """
      isRight m `shouldEqual` true

  describe "Comments" do
    it "should parse LineComment" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[],"comments":[{"LineComment":"line"}],"foreign":[]}
      """
      isRight m `shouldEqual` true

    it "should parse BlockComment" do
      let m = runExcept $ moduleFromJSON """
        {"moduleName":["Example","Main"],"imports":[],"builtWith":"0","modulePath":"src/Example/Main.purs","exports":[],"decls":[],"comments":[{"BlockComment":"block"}],"foreign":[]}
      """
      isRight m `shouldEqual` true
