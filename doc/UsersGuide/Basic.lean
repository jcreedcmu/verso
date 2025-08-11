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

-- inductive Foo where
--  | foo : String → Foo
--  | kids : List Foo → Foo

-- deriving instance Repr for Foo

-- /-- info: Foo.kids [Foo.foo "hi"] -/
-- #guard_msgs in
--  #eval  (Foo.kids [Foo.foo "hi"])


set_option pp.rawOnError true


mutual


def inline2str : Verso.Doc.Inline Manual → String
| .text s => s!"text[ {Repr.reprPrec s 0} ]"
| .emph x => s!"emph[ {x.map inline2str } ]"
| .bold x => s!"bold[ {x.map inline2str } ]"
| .code x => s!"code[ {Repr.reprPrec x 0} ]"
| .other container x => s!"other[{Lean.ToJson.toJson container} {x.map inline2str} ]"

  -- | code (string : String)
  -- /-- Embedded blobs of TeX math -/
  -- | math (mode : MathMode) (string : String)
  -- | linebreak (string : String)
  -- | link (content : Array (Inline genre)) (url : String)
  -- | footnote (name : String) (content : Array (Inline genre))
  -- | image (alt : String) (url : String)
  -- | concat (content : Array (Inline genre))
  -- | other (container : genre.Inline) (content : Array (Inline genre))

| _ => "unknown inline"
def block2str : Verso.Doc.Block Manual → String
| .code s => s!"CODE[ {s} ]"
| .para ins => s!"PARA[ {ins.map inline2str} ]"
| .concat ins => s!"CONCAT[ {ins.map block2str} ]"
| .other container content => s!"OTHER[{container.name} {content.map block2str }]"
| _ => "unknown block"

end

instance : ToString (Verso.Doc.Inline Manual) where
  toString x := inline2str x

instance : ToString (Verso.Doc.Block Manual) where
  toString x := block2str x

-- Why can't I use dot notation with x.toString below?
instance : ToString (Verso.Doc.Part Manual) where
  toString p := s!"... { p.titleString } { p.content.map (fun x => ToString.toString x) } ... "

--------------------

/-- This is a docstring.

Here's some more text with a `code inline` in it.
Here's when a `code inline`
occurs right before a line break.

And then here's a paragraph break.
-/
def foo := Unit

#docs (Manual) n "Writing Docs" :=
:::::::

%%%
shortTitle := "Documentation with Verso"
authors := ["David Thrane Christiansen"]
%%%

{docstring foo}

:::::::

/--
info: ... Writing Docs #[CONCAT[ #[OTHER[Verso.Genre.Manual.Block.docstring #[PARA[ #[text[ "This is a docstring." ]] ], PARA[ #[text[ "Here's some more text with a " ], other[{"name": "Verso.Genre.Manual.leanFromMarkdown",
 "id": null,
 "data":
 {"seq":
  {"highlights":
   [{"token": {"tok": {"kind": "unknown", "content": "code"}}},
    {"text": {"str": " "}},
    {"token": {"tok": {"kind": "unknown", "content": "inline"}}}]}}} #[code[ "code inline" ]] ], text[ " in it." ], text[ "\n" ], text[ "Here's when a " ], other[{"name": "Verso.Genre.Manual.leanFromMarkdown",
 "id": null,
 "data":
 {"seq":
  {"highlights":
   [{"token": {"tok": {"kind": "unknown", "content": "code"}}},
    {"text": {"str": " "}},
    {"token": {"tok": {"kind": "unknown", "content": "inline"}}}]}}} #[code[ "code inline" ]] ], text[ "\n" ], text[ "occurs right before a line break." ]] ], PARA[ #[text[ "And then here's a paragraph break." ]] ]]]] ]] ...
-/
#guard_msgs in
 #eval n

open Verso Doc
def block : Block Manual := Block.para #[]
def part : Part Manual := Part.mk #[] "title" none #[block] #[]

#doc (Manual) "Writing Documentation in Lean with Verso" =>

%%%
shortTitle := "Documentation with Verso"
authors := ["David Thrane Christiansen"]
%%%


{docstring foo}
