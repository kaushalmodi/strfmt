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
  exec "cp -f ./docs/strfmt_orig.rst ./docs/strfmt.rst"
  exec "nim doc strfmt.nim"
  exec "mv strfmt.html ./docs/index.html"

task docsorg, "Generate HTML docs using the Org file":
  exec "pandoc ./docs/strfmt.org -o ./docs/strfmt.rst"
  exec "nim doc strfmt.nim" # this fails at the moment -- Fri Jun 29 13:27:28 EDT 2018 - kmodi
  exec "mv strfmt.html ./docs/index.html"

task test, "Runs the tests in strfmt.nim":
  exec "rm -rf ./nimcache"
  exec "nim c -r strfmt.nim"
