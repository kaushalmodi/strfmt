# Package

version = "0.9.1"
author = "Frank Fischer"
description = "A string formatting library inspired by Python's `format`"
license = "MIT"

# Dependencies

requires "nim >= 0.18.1"

task test, "Runs the tests in strfmt.nim":
  exec "nim c -r strfmt.nim"
