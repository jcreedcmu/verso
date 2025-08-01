import VersoManual
import UsersGuide.Markup

open Verso.Genre Manual

def z := #[Verso.Doc.Arg.anon
    (Verso.Doc.ArgVal.name
      { raw := Lean.Syntax.ident
                 (Lean.SourceInfo.original "".toSubstring { byteIdx := 453 } "".toSubstring { byteIdx := 456 })
                 "foo".toSubstring
                 `foo
                 [] })]
def w := docstring z #[]

/-- This is a docstring.

Here's some more text with a `verbatim` token.
Here's when a `literal`
occurs right before a line break.

And then here's a paragraph break.

-/
def foo := Unit

set_option pp.rawOnError true

#doc (Manual) "Writing Documentation in Lean with Verso" =>

%%%
shortTitle := "Documentation with Verso"
authors := ["David Thrane Christiansen"]
%%%


{docstring foo}
