# Package

version = "0.10"
author = "Frank Fischer"
description = "A string formatting library inspired by Python's `format`"
license = "MIT"

# Dependencies

requires "nim >= 0.18.0"

import ospaths # for `/`
let
  pkgName = "strfmt"
  orgFile = "docs" / (pkgName & ".org")
  pandocLuaFilter = "--lua-filter " & ("docs" / "enumerated-lists.lua")
  rstFile = "docs" / (pkgName & ".rst")
  rstFileAuto = "docs" / (pkgName & "_autogen.rst")
  rstFileOrig = "docs" / (pkgName & "_orig.rst")
  htmlFileNimDoc = pkgName & ".html"
  htmlFileIndex = "docs" / "index.html"

# Cannot use "doc" as a task name as it's one of the inbuilt switches
# for nimble.
task docs, "Generate HTML docs using the Org file":
  # https://github.com/jgm/pandoc/issues/4749
  exec "pandoc " & pandocLuaFilter & " " & orgFile & " -o " & rstFile
  exec "nim doc " & pkgName
  mvFile rstFile, rstFileAuto
  mvFile htmlFileNimDoc, htmlFileIndex

task docsrst, "Generate HTML docs using the original rst file (not maintained)":
  cpFile rstFileOrig, rstFile
  exec "nim doc " & pkgName
  rmFile rstFile
  mvFile htmlFileNimDoc, htmlFileIndex

task test, "Runs the tests in " & pkgName & ".nim":
  rmDir "nimcache"
  exec "nim c -r " & pkgName


# https://nim-lang.org/docs/nimscript.html
