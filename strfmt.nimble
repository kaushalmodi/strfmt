# Package

version = "0.9.1"
author = "Frank Fischer"
description = "A string formatting library inspired by Python's `format`"
license = "MIT"

# Dependencies

requires "nim >= 0.18.1"

# Cannot use "doc" as a task name as it's one of the inbuilt switches
# for nimble.
task docs, "Generate HTML docs":
  exec "nim doc strfmt.nim"

task test, "Runs the tests in strfmt.nim":
  exec "nim c -r strfmt.nim"
