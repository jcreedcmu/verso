import Lean
open Lean
open Parser (ParserFn Parser atomic termParser optionalFn)

def strFn (str : String) : ParserFn := fun c s =>
    let rec go (iter : String.Iterator) (s : Parser.ParserState) :=
      if iter.atEnd then s
      else
        let ch := iter.curr
        go iter.next <| Parser.satisfyFn (· == ch) ch.toString c s
    go str.iter s


def myParser : Parser where
  fn := (atomic Parser.termParser).fn >> optionalFn (strFn ",") >> optionalFn (strFn "\n") >>
        strFn "%%\n"

@[combinator_parenthesizer myParser] def myParser.parenthesizer := PrettyPrinter.Parenthesizer.visitToken
@[combinator_formatter myParser] def myParser.formatter := PrettyPrinter.Formatter.visitAtom Name.anonymous

elab "#mytest" p:myParser "::::::" : command => do
  IO.println (s!"mytest output: {p}")

#mytest
abc,
%%
::::::

#mytest
abc
%%
::::::
