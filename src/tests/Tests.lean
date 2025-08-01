/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import VersoSearch.PorterStemmer
import VersoManual


def testTexMain : IO Unit := open Verso Genre Manual in do
 let logError (msg : String) := IO.eprintln msg
 let cfg : Config := {
   destination := "/tmp/_out",
   emitTeX := true,
   emitHtmlMulti := false,
   }
 let part : Doc.Part Manual := Doc.Part.mk #[] "title" none #[] #[]
 let exts : ExtensionImpls := (ExtensionImpls.fromLists [] [])
 let z2 := #[Verso.Doc.Arg.anon
    (Verso.Doc.ArgVal.name
      { raw := Lean.Syntax.ident
                 (Lean.SourceInfo.original "".toSubstring { byteIdx := 453 } "".toSubstring { byteIdx := 456 })
                 "foo".toSubstring
                 `foo
                 [] })]
 let w := docstring z2 #[]

 let z := ReaderT.run (emitTeX logError cfg part) exts
 _ ← z
 return

open Verso.Search.Stemmer.Porter in
def testStemmer : IO Unit := do
  let voc := include_str "stemmer/voc.txt"
  let output := include_str "stemmer/output.txt"

  let data := voc.splitOn "\n"
  let outData := output.splitOn "\n"

  let mut failures := #[]
  for x in data, y in outData do
    let s := porterStem x
    unless s == y do
      failures := failures.push (x, s, y)
  unless failures.isEmpty do
    IO.eprintln s!"{failures.size} failures"
    for (x, s, y) in failures do
      IO.eprintln s!"{x} --> {s} (wanted '{y}')"
    throw <| IO.userError "Stemmer tests failed"

def tests := [testStemmer, testTexMain]

def main : IO UInt32 := do
  let mut failures := 0
  for test in tests do
    try
      test
    catch
      | e => do
        IO.eprintln e
        failures := failures + 1
  if failures == 0 then
    IO.println "All tests passed"
  return failures
