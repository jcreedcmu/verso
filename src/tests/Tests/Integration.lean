/-
Copyright (c) 2025 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Jason Reed
-/
import Lean.Util.Diff

namespace Verso.Integration

/-- Configuration for the test runner -/
structure Config where
  /-- Where are expected files located? We expect a subdirectory
  `expected` and `runTest` should produce files into a subdirectory
  `output`. -/
  testDir : System.FilePath
  /-- Should the expected output be replaced with the actual output? -/
  updateExpected : Bool := false
  /-- How to run the test -/
  runTest : IO Unit

/-- Statistics for a test run. -/
structure TestStats where
  /-- The number of passing tests. -/
  passed : Nat := 0
  /-- The number of failing tests. -/
  failed : Nat := 0
  /-- The number of test that couldn't run. -/
  errors : Nat := 0

/-- The total number of tests from a given run. -/
def TestStats.total (stats : TestStats) : Nat :=
  stats.passed + stats.failed + stats.errors

/-- Print final statistics -/
def printStats (stats : TestStats) : IO Unit := do
  let total := stats.total
  IO.println ""
  IO.println s!"Tests run: {total}"
  IO.println s!"Passed: {stats.passed}"
  if stats.failed > 0 then
    IO.println s!"Failed: {stats.failed}"
  if stats.errors > 0 then
    IO.println s!"Errors: {stats.errors}"

  if stats.failed == 0 && stats.errors == 0 then
    IO.println "All tests passed! ✓"
  else
    IO.println s!"Some tests failed. ✗"

/-- Main test runner -/
def runTests (config : Config) : IO Unit := do
  unless ← System.FilePath.pathExists config.testDir do
    throw <| .userError s!"Test directory not found: {config.testDir}"

  config.runTest

  -- let inputFiles ← findInputFiles config.testDir

  -- if inputFiles.isEmpty then
  --   IO.println s!"No .input files found in {config.testDir}"
  --   return

  -- if config.updateExpected then
  --   IO.println s!"Updating expected outputs in {config.testDir}..."
  -- else
  --   IO.println s!"Running tests in {config.testDir}..."
  -- IO.println ""

  let mut stats : TestStats := {}
  -- for inputFile in inputFiles do
  --   let result ← runSingleTest config inputFile
  --   result.print
  --   stats := stats.add result

  printStats stats

  if stats.failed == 0 && stats.errors == 0 then return
  else throw <| .userError s!"Failed with {stats.failed} failures and {stats.errors} errors"
