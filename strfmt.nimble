# Package

version = "0.9.1"
author = "Frank Fischer"
description = "A string formatting library inspired by Python's `format`"
license = "MIT"

# Dependencies

requires "nim >= 0.18.1"

# Cannot use "doc" as a task name as it's one of the inbuilt switches
# for nimble.
task docs, "Generate HTML docs using the original rst file":
  exec "cp -f ./doc/strfmt_orig.rst ./doc/strfmt.rst"
  exec "nim doc strfmt.nim"

task docsorg, "Generate HTML docs using the Org file":
  exec "pandoc ./doc/strfmt.org -o ./doc/strfmt.rst"
  exec "nim doc strfmt.nim" # this fails at the moment.

task test, "Runs the tests in strfmt.nim":
  exec "rm -rf ./nimcache"
  exec "nim c -r strfmt.nim"
