import Lean
import Verso
import VersoManual

open Verso Genre Manual
open Lean
open Parser (ParserFn Parser)

def myStrFn (str : String) : ParserFn := fun c s =>
    let rec go (iter : String.Iterator) (s : Parser.ParserState) :=
      if iter.atEnd then s
      else
        let ch := iter.curr
        go iter.next <| Parser.satisfyFn (· == ch) ch.toString c s
    go str.iter s

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
