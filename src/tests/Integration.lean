import Verso
import VersoManual
open Verso Doc

def testTexMain (block : Block Genre.Manual) : IO Unit := open Verso Genre Manual in do
 let logError (msg : String) := IO.eprintln msg
 let cfg : Config := {
   destination := "/tmp/_out",
   emitTeX := true,
   emitHtmlMulti := false,
   }
 let part : Doc.Part Manual := Doc.Part.mk #[] "this is an integration test title" none #[block] #[]
 let exts : ExtensionImpls := by exact extension_impls% -- FIXME: pick out just the extension impls needed?
 let z := ReaderT.run (emitTeX logError cfg part) exts
 _ ← z
 return

def main : IO Unit := do
 let custom :  Genre.Manual.Inline :=  { name := `Verso.Genre.Manual.leanFromMarkdown  }
 let block : Block Genre.Manual := Block.para #[Inline.text "Here's some text",
      Inline.other custom #[Inline.code "code inline"],
      Inline.text "\n", Inline.text "Some more text"]
 testTexMain block
