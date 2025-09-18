import Lean
import Verso
import VersoManual

open Verso Genre Manual
open Lean
open Parser (ParserFn sepByFn manyFn blankLine atomicFn Parser nodeFn
             takeWhileFn eatSpaces ignoreFn chFn eoiFn bolThen strFn ppGroup many)

private def asStringAux  : ParserFn := fun _c s => s.pushSyntax (    mkAtom "foo" )

/-- Match an arbitrary Parser and return the consumed String in a `Syntax.atom`. -/
def asStringFn (p : ParserFn) : ParserFn := fun c s =>
  let iniSz := s.stxStack.size
  let s := p c s
  if s.hasError then s
  else asStringAux c (s.shrinkStack iniSz)

def myStrFn (str : String) : ParserFn := asStringFn <| fun c s =>
  let rec go (iter : String.Iterator) (s : Parser.ParserState) :=
    if iter.atEnd then s
    else
      let ch := iter.curr
      go iter.next <| Parser.satisfyFn (· == ch) ch.toString c s
  let iniPos := s.pos
  let iniSz := s.stxStack.size
  let s := go str.iter s
  if s.hasError then s.mkErrorAt s!"'{str}'" iniPos (some iniSz) else s


open Lean.Parser.Term in
def myContents : ParserFn :=
    (Parser.atomic Parser.termParser).fn >> Parser.optionalFn (myStrFn ",") >>
    myStrFn "%%\n"

def myParser : Parser where
  fn := myContents

@[combinator_parenthesizer myParser] def myParser.parenthesizer := PrettyPrinter.Parenthesizer.visitToken
@[combinator_formatter myParser] def myParser.formatter := PrettyPrinter.Formatter.visitAtom Name.anonymous

elab "#mytest" p:myParser "::::::" : command => do
  IO.println (s!"mytest output: {p}")

#mytest
abc,%%
::::::


#mytest
abc%%
::::::
