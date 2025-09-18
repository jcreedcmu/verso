import Lean
import Verso
import VersoManual

open Verso Genre Manual
open Lean

open Parser (ParserFn sepByFn manyFn blankLine atomicFn Parser nodeFn takeWhileFn eatSpaces ignoreFn chFn eoiFn bolThen strFn ppGroup many)
open Parser.Term (structInstLVal)

meta def fooParser : Parser where
  fn := strFn "foo"


def structInstField := ppGroup <| leading_parser
  Parser.ident >> Parser.Term.structInstFieldDeclParser


meta def contents : Parser :=
   (Parser.sepByIndent structInstField ", " (allowTrailingSep := true ))

open Lean.Parser.Term in
def metadataBlock : ParserFn :=
  nodeFn ``Doc.Syntax.metadata_block <|
    opener >>
    contents.fn >>
    closer
where
  opener := strFn "%%%" >> ignoreFn (chFn '\n')
  closer := strFn "%%%" >> ignoreFn (chFn '\n')

def docco : Parser where
  fn := metadataBlock

@[combinator_parenthesizer docco] def docco.parenthesizer := PrettyPrinter.Parenthesizer.visitToken
@[combinator_formatter docco] def docco.formatter := PrettyPrinter.Formatter.visitAtom Name.anonymous


elab "#mydocs" text:docco "::::::" : command => do
  IO.println (s!"mydocs output: {text}")

#mydocs
%%%
foo := "bar"
baz := "blap",
%%%
::::::

elab "#debug" txt:str : command => do
  IO.println (← docco.fn.test txt.getString)

#debug r#"%%%
foo := "bar"
baz := "blap"
%%%
"#

#mydocs
%%%
foo := "bar"
baz := "blap"
%%%
::::::
