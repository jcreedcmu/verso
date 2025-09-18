import Lean
import Verso
import VersoManual

open Verso Genre Manual
open Lean
open Parser (ParserFn sepByFn manyFn blankLine atomicFn Parser nodeFn
             takeWhileFn eatSpaces ignoreFn chFn eoiFn bolThen strFn ppGroup many)

open Lean.Parser.Term in
def myContents : ParserFn :=
    (Parser.sepBy (Parser.atomic Parser.termParser) "," (allowTrailingSep := true )).fn >>
    strFn "%%%" >> ignoreFn (chFn '\n')

def myParser : Parser where
  fn := myContents

@[combinator_parenthesizer myParser] def myParser.parenthesizer := PrettyPrinter.Parenthesizer.visitToken
@[combinator_formatter myParser] def myParser.formatter := PrettyPrinter.Formatter.visitAtom Name.anonymous

elab "#mytest" p:myParser "::::::" : command => do
  IO.println (s!"mytest output: {p}")

#mytest
identifier,
%%%
::::::

elab "#debug" txt:str : command => do
  IO.println (← myParser.fn.test txt.getString)

#debug r#"identifier %%%
"#

#mytest
identifier
%%%
::::::
