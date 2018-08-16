# Copyright (c) 2014, 2015, 2016, 2017, 2018 Frank Fischer <frank-fischer@shadow-soft.de>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

## :Author: Frank Fischer
## :License: MIT
## :Version: 0.10
##
## Introduction
## ============
##
## Basic helpers for formatting values as strings in a configurable
## way. It is inspired by and similar to `Python's format function
## <https://docs.python.org/3.4/library/functions.html#format>`_.
##
## .. include:: ./docs/strfmt.rst
##

import macros
import options

from strutils import IdentStartChars, Digits
from parseutils import parseInt, parseIdent, parseUntil, skipUntil
from unicode import Rune, runeLen, runeLenAt, runeAt, toUTF8
from math import classify, FloatClass, floor, log10, pow
from streams import Stream, write

type
  FormatError* = object of Exception ## Error in the format string.

  Writer* = concept W
    ## Writer to output a character `c`.
    write(W, ' ')

  FmtAlign* = enum ## Format alignment
    faDefault  ## default for given format type
    faLeft     ## left aligned
    faRight    ## right aligned
    faCenter   ## centered
    faPadding  ## right aligned, fill characters after sign (numbers only)

  FmtSign* = enum ## Format sign
    fsMinus    ## only unary minus, no reserved sign space for positive numbers
    fsPlus     ## unary minus and unary plus
    fsSpace    ## unary minus and reserved space for positive numbers

  FmtType* = enum ## Format type
    ftDefault  ## default format for given parameter type
    ftStr      ## string
    ftChar     ## character
    ftDec      ## decimal integer
    ftBin      ## binary integer
    ftOct      ## octal integer
    ftHex      ## hexadecimal integer
    ftFix      ## real number in fixed point notation
    ftSci      ## real number in scientific notation
    ftGen      ## real number in generic form (either fixed point or scientific)
    ftPercent  ## real number multiplied by 100 and % added

  Format* = tuple           ## Formatting information.
    typ: FmtType            ## format type
    precision: int          ## floating point precision
    width: int              ## minimal width
    fill: string            ## the fill character, UTF8
    align: FmtAlign         ## alignment
    sign: FmtSign           ## sign notation
    baseprefix: bool        ## whether binary, octal, hex should be prefixed by 0b, 0x, 0o
    upcase: bool            ## upper case letters in hex or exponential formats
    comma: bool             ##
    arysep: Option[string]  ## separator for array elements

  PartKind = enum pkStr, pkFmt

  Part = object
    ## Information of a part of the target string.
    case kind: PartKind ## type of the part
    of pkStr:
      str: string ## literal string
    of pkFmt:
      arg: int ## position argument
      fmt: string ## format string
      field: string ## field of argument to be accessed
      index: int ## array index of argument to be accessed
      nested: bool ## true if the argument contains nested formats

# SomeFloat replaced SomeReal in Nim 0.18.1.
when not declared SomeFloat:
  type
    SomeFloat = SomeReal

const
  DefaultPrec = 6 ## Default precision for floating point numbers.
  DefaultFmt*: Format = (ftDefault, -1, -1, "", faDefault, fsMinus, false, false, false, some("")) ## \
    ## Default format corresponding to the empty format string, i.e.
    ##   `x.format("") == x.format(DefaultFmt)`.
  round_nums = [0.5, 0.05, 0.005, 0.0005, 0.00005, 0.000005, 0.0000005, 0.00000005, 0.000000005, 0.0000000005]
    ## Rounding offset for floating point numbers up to precision 10.

proc write*(s: var string; c: char) =
  s.add(c)

proc parse*(fmt: string): Format {.nosideeffect.} =
  ## Converts the format string `fmt` into a `Format` structure.
  let n = fmt.len
  var pos = 0

  # alignment and possible fill character
  if pos == n:
    result.align = faDefault
  else:
    let rlen = fmt.runeLenAt(pos)
    if pos + rlen < fmt.len and fmt[pos + rlen] in { '<', '>', '^', '=' }:
      result.fill = fmt[pos ..< (pos+rlen)]
      case fmt[pos + rlen]
      of '<': result.align = faLeft
      of '>': result.align = faRight
      of '^': result.align = faCenter
      of '=': result.align = faPadding
      else: assert(false)
      pos.inc rlen + 1
    elif fmt[pos] in { '<', '>', '^', '=' }:
      case fmt[pos]
      of '<': result.align = faLeft
      of '>': result.align = faRight
      of '^': result.align = faCenter
      of '=': result.align = faPadding
      else: assert(false)
      pos.inc
    else:
      result.align = faDefault

  # sign
  if pos < n and fmt[pos] in { '-', '+', ' '}:
    case fmt[pos]
    of '-': result.sign = fsMinus
    of '+': result.sign = fsPlus
    of ' ': result.sign = fsSpace
    else: discard
    pos.inc
  else:
    result.sign = fsMinus

  # base prefix
  if pos < n and fmt[pos] == '#':
    result.baseprefix = true
    pos.inc

  # zero padding
  if pos < n and fmt[pos] == '0':
    if result.fill != "":
      raise newException(FormatError, "Leading 0 in with not allowed with explicit fill character")
    if result.align != faDefault:
      raise newException(FormatError, "Leading 0 in with not allowed with explicit alignment")
    result.fill = "0"
    result.align = faPadding
    pos.inc

  # width
  if pos < n and fmt[pos] in Digits:
    pos.inc parseInt(fmt, result.width, pos)
  else:
    result.width = -1

  # comma
  if pos < n and fmt[pos] == ',':
    result.comma = true
    pos.inc

  # precision
  if pos < n and fmt[pos] == '.':
    pos.inc
    if not (pos < n and fmt[pos] in Digits):
      raise newException(FormatError, "Expected number describing precision after '.'")
    pos += parseInt(fmt, result.precision, pos)
  else:
    result.precision = -1

  # type
  if pos < n:
    case fmt[pos]
    of 's': result.typ = ftStr
    of 'c': result.typ = ftChar
    of 'd', 'n': result.typ = ftDec
    of 'b': result.typ = ftBin
    of 'o': result.typ = ftOct
    of 'x': result.typ = ftHex
    of 'X': result.typ = ftHex; result.upcase = true
    of 'f', 'F': result.typ = ftFix
    of 'e': result.typ = ftSci
    of 'E': result.typ = ftSci; result.upcase = true
    of 'g': result.typ = ftGen
    of 'G': result.typ = ftGen; result.upcase = true
    of '%': result.typ = ftPercent
    of 'a': pos.dec # array format, handled below
    else:
      raise newException(FormatError, "Unknown format type: '" & $fmt[pos] & "'")
    pos.inc
  else:
    result.typ = ftDefault

  # array separator
  if pos < n and fmt[pos] == 'a':
    result.arysep = some(fmt[pos+1 .. fmt.high])
    pos = fmt.len
  else:
    result.arysep = none(string)

  # end of format string
  if pos < n:
    raise newException(FormatError, "Unexpected character at the end of the format string: '" & fmt[pos] & "'")

proc getalign*(fmt: Format; defalign: FmtAlign; slen: int) : tuple[left, right:int] {.nosideeffect.} =
  ## Returns the number of left and right padding characters for a
  ## given format alignment and width of the object to be printed.
  ##
  ## `fmt`
  ##    the format data
  ## `default`
  ##    if `fmt.align == faDefault`, then this alignment is used
  ## `slen`
  ##    the width of the object to be printed.
  ##
  ## The returned values `(left, right)` will be as minimal as possible
  ## so that `left + slen + right >= fmt.width`.
  result.left = 0
  result.right = 0
  if (fmt.width >= 0) and (slen < fmt.width):
    let alg = if fmt.align == faDefault: defalign else: fmt.align
    case alg:
    of faLeft: result.right = fmt.width - slen
    of faRight, faPadding: result.left = fmt.width - slen
    of faCenter:
      result.left = (fmt.width - slen) div 2
      result.right = fmt.width - slen - result.left
    else: discard

proc writefill(o: var Writer; fmt: Format; n: int; signum: int = 0) =
  ## Write characters for filling. This function also writes the sign
  ## of a numeric format and handles the padding alignment
  ## accordingly.
  ##
  ## `o`
  ##   output object
  ## `add`
  ##   output function
  ## `fmt`
  ##   format to be used (important for padding alignment)
  ## `n`
  ##   the number of filling characters to be written
  ## `signum`
  ##   the sign of the number to be written, < 0 negative, > 0 positive, = 0 zero
  if fmt.align == faPadding and signum != 0:
    if signum < 0: write(o, '-')
    elif fmt.sign == fsPlus: write(o, '+')
    elif fmt.sign == fsSpace: write(o, ' ')

  if fmt.fill == "":
    for i in 1 .. n: write(o, ' ')
  else:
    for i in 1 .. n:
      for c in fmt.fill:
        write(o, c)

  if fmt.align != faPadding and signum != 0:
    if signum < 0: write(o, '-')
    elif fmt.sign == fsPlus: write(o, '+')
    elif fmt.sign == fsSpace: write(o, ' ')

proc writeformat*(o: var Writer; s: string; fmt: Format) =
  ## Write string `s` according to format `fmt` using output object
  ## `o` and output function `add`.
  if fmt.typ notin {ftDefault, ftStr}:
    raise newException(FormatError, "String variable must have 's' format type")

  # compute alignment
  let l = if fmt.precision < 0: runelen(s) else: min(runelen(s), fmt.precision)
  var alg = getalign(fmt, faLeft, l)
  writefill(o, fmt, alg.left)
  var pos = 0
  for i in 0 ..< l:
    let rlen = runeLenAt(s, pos)
    for j in pos ..< (pos+rlen): write(o, s[j])
    pos += rlen
  writefill(o, fmt, alg.right)

proc writeformat*(o: var Writer; c: char; fmt: Format) =
  ## Write character `c` according to format `fmt` using output object
  ## `o` and output function `add`.
  if not (fmt.typ in {ftChar, ftDefault}):
    raise newException(FormatError, "Character variable must have 'c' format type")

  # compute alignment
  var alg = getalign(fmt, faLeft, 1)
  writefill(o, fmt, alg.left)
  write(o, c)
  writefill(o, fmt, alg.right)

proc writeformat*(o: var Writer; c: Rune; fmt: Format) =
  ## Write rune `c` according to format `fmt` using output object
  ## `o` and output function `add`.
  if not (fmt.typ in {ftChar, ftDefault}):
    raise newException(FormatError, "Character variable must have 'c' format type")

  # compute alignment
  var alg = getalign(fmt, faLeft, 1)
  writefill(o, fmt, alg.left)
  let s = c.toUTF8
  for c in s: write(o, c)
  writefill(o, fmt, alg.right)

proc abs(x: SomeUnsignedInt): SomeUnsignedInt {.inline.} = x
  ## Return the absolute value of the unsigned int `x`.

proc writeformat*(o: var Writer; i: SomeInteger; fmt: Format) =
  ## Write integer `i` according to format `fmt` using output object
  ## `o` and output function `add`.
  var fmt = fmt
  if fmt.typ == ftDefault:
    fmt.typ = ftDec
  if not (fmt.typ in {ftBin, ftOct, ftHex, ftDec}):
    raise newException(FormatError, "Integer variable must of one of the following types: b,o,x,X,d,n")

  var base: type(i)
  var len = 0
  case fmt.typ:
  of ftDec:
    base = 10
  of ftBin:
    base = 2
    if fmt.baseprefix: len += 2
  of ftOct:
    base = 8
    if fmt.baseprefix: len += 2
  of ftHex:
    base = 16
    if fmt.baseprefix: len += 2
  else: assert(false)

  if fmt.sign != fsMinus or i < 0: len.inc

  var x: type(i) = abs(i)
  var istr: array[0 .. 31, uint8]
  var ilen = 0
  const COMMA = 42.uint8 # a marker for a comma
  while x > 0.SomeInteger:
    if fmt.comma and ilen mod 4 == 3:
      istr[ilen] = COMMA
      ilen.inc
      len.inc
    istr[ilen] = (x mod base).uint8
    len.inc
    ilen.inc
    x = x div base
  if ilen == 0:
    istr[ilen] = 0
    ilen.inc
    len.inc

  var alg = getalign(fmt, faRight, len)
  writefill(o, fmt, alg.left, if i >= 0.SomeInteger: 1 else: -1)
  if fmt.baseprefix:
    case fmt.typ
    of ftBin:
      write(o, '0')
      write(o, 'b')
    of ftOct:
      write(o, '0')
      write(o, 'o')
    of ftHex:
      write(o, '0')
      write(o, 'x')
    else:
      raise newException(FormatError, "# only allowed with b, o, x or X")
  while ilen > 0:
    ilen.dec
    let c = istr[ilen]
    if c < 10:
      write(o, ('0'.int + c.int).char)
    elif c == COMMA:
      write(o, ',')
    elif fmt.upcase:
      write(o, ('A'.int + c.int - 10).char)
    else:
      write(o, ('a'.int + c.int - 10).char)
  writefill(o, fmt, alg.right)

proc writeformat*(o: var Writer; p: pointer; fmt: Format) =
  ## Write pointer `i` according to format `fmt` using output object
  ## `o` and output function `add`.
  ##
  ## Pointers are casted to unsigned int and formatted as hexadecimal
  ## with prefix unless specified otherwise.
  var f = fmt
  if f.typ == 0.char:
    f.typ = 'x'
    f.baseprefix = true
  writeformat(o, add, cast[uint](p), f)

proc writeformat*(o: var Writer; x: SomeFloat; fmt: Format) =
  ## Write real number `x` according to format `fmt` using output
  ## object `o` and output function `add`.
  var fmt = fmt
  # handle default format
  if fmt.typ == ftDefault:
    fmt.typ = ftGen
    if fmt.precision < 0: fmt.precision = DefaultPrec
  if not (fmt.typ in {ftFix, ftSci, ftGen, ftPercent}):
    raise newException(FormatError, "Integer variable must of one of the following types: f,F,e,E,g,G,%")

  let positive = x >= 0 and classify(x) != fcNegZero
  var len = 0

  if fmt.sign != fsMinus or not positive: len.inc

  var prec = if fmt.precision < 0: DefaultPrec else: fmt.precision
  var y = abs(x)
  var exp = 0
  var numstr, frstr: array[0 .. 31, char]
  var numlen, frbeg, frlen = 0

  if fmt.typ == ftPercent: y *= 100

  case classify(x):
  of fcNan:
    numstr[0 .. 2] = ['n', 'a', 'n']
    numlen = 3
  of fcInf, fcNegInf:
    numstr[0 .. 2] = ['f', 'n', 'i']
    numlen = 3
  else: # a usual fractional number
    if not (fmt.typ in {ftFix, ftPercent}): # not fixed point
      if y != 0:
        exp = int(floor(log10(y)))
      else:
        exp = 0
      if fmt.typ == ftGen:
        if prec == 0: prec = 1
        if -4 <= exp and exp < prec:
          prec = prec-1-exp
          exp = 0
        else:
          prec = prec - 1
          len += 4 # exponent
      else:
        len += 4 # exponent
      # shift y so that 1 <= abs(y) < 2
      if exp > 0: y /= pow(10.SomeFloat, abs(exp).SomeFloat)
      elif exp < 0: y *= pow(10.SomeFloat, abs(exp).SomeFloat)
    elif fmt.typ == ftPercent:
      len += 1 # percent sign

    # handle rounding by adding +0.5 * LSB
    if prec < len(round_nums): y += round_nums[prec]

    # split into integer and fractional part
    var mult = 1'i64
    for i in 1 .. prec: mult *= 10
    var num = y.int64
    var fr = ((y - num.SomeFloat) * mult.SomeFloat).int64
    # build integer part string
    while num != 0:
      if fmt.comma and numlen mod 4 == 3:
        numstr[numlen] = ','
        numlen.inc
      numstr[numlen] = ('0'.int + (num mod 10)).char
      numlen.inc
      num = num div 10
    if numlen == 0:
      numstr[0] = '0'
      numlen.inc
    # build fractional part string
    while fr != 0:
      frstr[frlen] = ('0'.int + (fr mod 10)).char
      frlen.inc
      fr = fr div 10
    while frlen < prec:
      frstr[frlen] = '0'
      frlen.inc
    # possible remove trailing 0
    if fmt.typ == ftGen:
      while frbeg < frlen and frstr[frbeg] == '0': frbeg.inc
  # update length of string
  len += numlen;
  if frbeg < frlen:
    len += 1 + frlen - frbeg # decimal point and fractional string

  let alg = getalign(fmt, faRight, len)
  writefill(o, fmt, alg.left, if positive: 1 else: -1)
  for i in (numlen-1).countdown(0): write(o, numstr[i])
  if frbeg < frlen:
    write(o, '.')
    for i in (frlen-1).countdown(frbeg): write(o, frstr[i])
  if fmt.typ == ftSci or (fmt.typ == ftGen and exp != 0):
    write(o, if fmt.upcase: 'E' else: 'e')
    if exp >= 0:
      write(o, '+')
    else:
      write(o, '-')
      exp = -exp
    if exp < 10:
      write(o, '0')
      write(o, ('0'.int + exp).char)
    else:
      var i=0
      while exp > 0:
        numstr[i] = ('0'.int + exp mod 10).char
        i+=1
        exp = exp div 10
      while i>0:
        i-=1
        write(o, numstr[i])
  if fmt.typ == ftPercent: write(o, '%')
  writefill(o, fmt, alg.right)

proc writeformat*(o: var Writer; b: bool; fmt: Format) =
  ## Write boolean value `b` according to format `fmt` using output
  ## object `o`. A boolean may be formatted numerically or as string.
  ## In the former case true is written as 1 and false as 0, in the
  ## latter the strings "true" and "false" are shown, respectively.
  ## The default is string format.
  if fmt.typ in {ftStr, ftDefault}:
    writeformat(o,
                if b: "true"
                else: "false",
                fmt)
  elif fmt.typ in {ftBin, ftOct, ftHex, ftDec}:
    writeformat(o,
                if b: 1
                else: 0,
                fmt)
  else:
    raise newException(FormatError, "Boolean values must of one of the following types: s,b,o,x,X,d,n")

proc writeformat*(o: var Writer; ary: openarray[any]; fmt: Format) =
  ## Write array `ary` according to format `fmt` using output object
  ## `o` and output function `add`.
  if ary.len == 0: return

  var sep: string
  var nxtfmt = fmt
  if isNone(fmt.arysep):
    sep = "\t"
  elif get(fmt.arysep).len == 0:
    sep = ""
  else:
    let sepch = get(fmt.arysep)[0]
    let nxt = 1 + skipUntil(get(fmt.arysep), sepch, 1)
    if nxt >= 1:
      nxtfmt.arysep = some(get(fmt.arysep).substr(nxt))
      sep = get(fmt.arysep).substr(1, nxt-1)
    else:
      nxtfmt.arysep = some("")
      sep = get(fmt.arysep).substr(1)
  writeformat(o, ary[0], nxtfmt)
  for i in 1 ..< ary.len:
    for c in sep: write(o, c)
    writeformat(o, ary[i], nxtfmt)

proc addformat*[T](o: var Writer; x: T; fmt: Format = DefaultFmt) {.inline.} =
  ## Write `x` formatted with `fmt` to `o`.
  writeformat(o, x, fmt)

proc addformat*[T](o: var Writer; x: T; fmt: string) {.inline.} =
  ## The same as `addformat(o, x, parse(fmt))`.
  addformat(o, x, fmt.parse)

proc addformat*(s: var string; x: string) {.inline.} =
  ## Write `x` to `s`. This is a fast specialized version for
  ## appending unformatted strings.
  add(s, x)

when not defined(js):
  proc addformat*(f: File; x: string) {.inline.} =
    ## Write `x` to `f`. This is a fast specialized version for
    ## writing unformatted strings to a file.
    write(f, x)

  proc addformat*[T](f: File; x: T; fmt: Format = DefaultFmt) {.inline.} =
    ## Write `x` to file `f` using format `fmt`.
    var g = f
    writeformat(g, x, fmt)

  proc addformat*[T](f: File; x: T; fmt: string) {.inline.} =
    ## Write `x` to file `f` using format string `fmt`. This is the same
    ## as `addformat(f, x, parse(fmt))`
    addformat(f, x, parse(fmt))

proc addformat*(s: Stream; x: string) {.inline.} =
  ## Write `x` to `s`. This is a fast specialized version for
  ## writing unformatted strings to a stream.
  write(s, x)

proc addformat*[T](s: Stream; x: T; fmt: Format = DefaultFmt) {.inline.} =
  ## Write `x` to stream `s` using format `fmt`.
  var g = s
  writeformat(g, x, fmt)

proc addformat*[T](s: Stream; x: T; fmt: string) {.inline.} =
  ## Write `x` to stream `s` using format string `fmt`. This is the same
  ## as `addformat(s, x, parse(fmt))`
  addformat(s, x, parse(fmt))

proc format*[T](x: T; fmt: Format): string =
  ## Return `x` formatted as a string according to format `fmt`.
  result = ""
  addformat(result, x, fmt)

proc format*[T](x: T; fmt: string): string =
  ## Return `x` formatted as a string according to format string `fmt`.
  result = format(x, fmt.parse)

proc format*[T](x: T): string {.inline.} =
  ## Return `x` formatted as a string according to the default format.
  ## The default format corresponds to an empty format string.
  var fmt {.global.} : Format = DefaultFmt
  result = format(x, fmt)

proc unquoted(s: string): string {.compileTime.} =
  ## Return `s` {{ and }} by single { and }, respectively.
  result = ""
  var pos = 0
  while pos < s.len:
    let nxt = pos + skipUntil(s, {'{', '}'})
    result.add(s.substr(pos, nxt))
    pos = nxt + 2

proc splitfmt(s: string): seq[Part] {.compiletime, nosideeffect.} =
  ## Split format string `s` into a sequence of "parts".
  ##

  ## Each part is either a literal string or a format specification. A
  ## format specification is a substring of the form
  ## "{[arg][:format]}" where `arg` is either empty or a number
  ## referring to the `arg`-th argument and an additional field or array
  ## index. The format string is a string accepted by `parse`.
  result = @[]
  var pos = 0
  while true:
    let oppos = pos + skipUntil(s, {'{', '}'}, pos)
    # reached the end
    if oppos >= s.len:
      if pos < s.len:
        result.add(Part(kind: pkStr, str: s.substr(pos).unquoted))
      return
    # skip double
    if oppos + 1 < s.len and s[oppos] == s[oppos+1]:
      result.add(Part(kind: pkStr, str: s.substr(pos, oppos)))
      pos = oppos + 2
      continue
    if s[oppos] == '}':
      error("Single '}' encountered in format string")
    if oppos > pos:
      result.add(Part(kind: pkStr, str: s.substr(pos, oppos-1).unquoted))
    # find matching closing }
    var lvl = 1
    var nested = false
    pos = oppos
    while lvl > 0:
      pos.inc
      pos = pos + skipUntil(s, {'{', '}'}, pos)
      if pos >= s.len:
        error("Single '{' encountered in format string")
      if s[pos] == '{':
        lvl.inc
        if lvl == 2:
          nested = true
        if lvl > 2:
          error("Too many nested format levels")
      else:
        lvl.dec
    let clpos = pos
    var fmtpart = Part(kind: pkFmt, arg: -1, fmt: s.substr(oppos+1, clpos-1), field: "", index: int.high, nested: nested)
    if fmtpart.fmt.len > 0:
      var pos = 0
      let n = fmtpart.fmt.len
      # argument number
      if pos < n and fmtpart.fmt[pos] in Digits:
        pos += parseInt(fmtpart.fmt, fmtpart.arg, pos)

      # field
      if pos < n and fmtpart.fmt[pos] == '.':
        pos.inc
        if pos == n or fmtpart.fmt[pos] notin IdentStartChars:
          error("Expected field identifier name after '.' in format string")
        pos += parseIdent(fmtpart.fmt, fmtpart.field, pos)

      # index
      if pos < n and fmtpart.fmt[pos] == '[':
        pos.inc
        let lenidx = parseInt(fmtpart.fmt, fmtpart.index, pos)
        if lenidx == 0:
          error("Expected numeric index after '[' in format string")
        pos.inc lenidx
        if pos == n or fmtpart.fmt[pos] != ']':
          error("Missing closing bracket ']' in format string")
        pos.inc

      # format specifier
      if pos < n and fmtpart.fmt[pos] == ':':
        fmtpart.fmt = fmtpart.fmt[(pos+1) ..< n]
        pos = n
      else:
        fmtpart.fmt = ""

      if pos < n: error("Unexpected end of format string")

    result.add(fmtpart)
    pos = clpos + 1

proc literal(s: string): NimNode {.compiletime, nosideeffect.} =
  ## Return the nim literal of string `s`. This handles the case if
  ## `s` is nil.
  result = newLit(s)

proc literal(b: bool): NimNode {.compiletime, nosideeffect.} =
  ## Return the nim literal of boolean `b`. This is either `true`
  ## or `false` symbol.
  result = if b: "true".ident else: "false".ident

proc literal[T](x: T): NimNode {.compiletime, nosideeffect.} =
  ## Return the nim literal of value `x`.
  when type(x) is enum:
    result = ($x).ident
  else:
    result = newLit(x)

proc generatefmt(fmtstr: string;
                 args: var openarray[tuple[arg:NimNode, cnt:int]];
                 arg: var int;): seq[tuple[val, fmt:NimNode]] {.compiletime.} =
  ## `fmtstr`
  ##   the format string
  ## `args`
  ##   array of expressions for the arguments
  ## `arg`
  ##   the number of the next argument for automatic parsing
  ##
  ## If `arg` is < 0 then the function assumes that explicit numbering
  ## must be used, otherwise automatic numbering is used starting at
  ## `arg`. The value of `arg` is updated according to the number of
  ## arguments being used. If `arg == 0` then automatic and manual
  ## numbering is not decided (because no explicit manual numbering is
  ## fixed and no automatically numbered argument has been used so
  ## far).
  ##
  ## The function returns a list of pairs `(val, fmt)` where `val` is
  ## an expression to be formatted and `fmt` is the format string (or
  ## Format). Therefore, the resulting string can be generated by
  ## concatenating expressions `val.format(fmt)`. If `fmt` is `nil`
  ## then `val` is a (literal) string expression.
  try:
    result = @[]
    for part in splitfmt(fmtstr):
      case part.kind
      of pkStr: result.add((newLit(part.str), nil))
      of pkFmt:
        # first compute the argument expression
        # start with the correct index
        var argexpr : NimNode
        if part.arg >= 0:
          if arg > 0:
            error("Cannot switch from automatic field numbering to manual field specification")
          if part.arg >= args.len:
            error("Invalid explicit argument index: " & $part.arg)
          argexpr = args[part.arg].arg
          args[part.arg].cnt = args[part.arg].cnt + 1
          arg = -1
        else:
          if arg < 0:
            error("Cannot switch from manual field specification to automatic field numbering")
          if arg >= args.len:
            error("Too few arguments for format string")
          argexpr = args[arg].arg
          args[arg].cnt = args[arg].cnt + 1
          arg.inc
        # possible field access
        if part.field != "":
          argexpr = newDotExpr(argexpr, part.field.ident)
        # possible array access
        if part.index < int.high:
          argexpr = newNimNode(nnkBracketExpr).add(argexpr, newLit(part.index))
        # now the expression for the format data
        var fmtexpr: NimNode
        if part.nested:
          # nested format string. Compute the format string by
          # concatenating the parts of the substring.
          for e in generatefmt(part.fmt, args, arg):
            var newexpr = if e.fmt == nil: e.val else: newCall(bindsym"format", e.val, e.fmt)
            if fmtexpr != nil and fmtexpr.kind != nnkNilLit:
              fmtexpr = infix(fmtexpr, "&", newexpr)
            else:
              fmtexpr = newexpr
        else:
          # literal format string, precompute the format data
          fmtexpr = newNimNode(nnkPar)
          for field, val in part.fmt.parse.fieldPairs:
            fmtexpr.add(newNimNode(nnkExprColonExpr).add(field.ident, literal(val)))
        # add argument
        result.add((argexpr, fmtexpr))
  finally:
    discard

proc addfmtfmt(fmtstr: string; args: NimNode; retvar: NimNode): NimNode {.compileTime.} =
  var argexprs = newseq[tuple[arg:NimNode; cnt:int]](args.len)
  result = newNimNode(nnkStmtListExpr)
  # generate let bindings for arguments
  for i in 0 ..< args.len:
    let argsym = gensym(nskLet, "arg" & $i)
    result.add(newLetStmt(argsym, args[i]))
    argexprs[i].arg = argsym
  # add result values
  var arg = 0
  for e in generatefmt(fmtstr, argexprs, arg):
    if e.fmt == nil or e.fmt.kind == nnkNilLit:
      result.add(newCall(bindsym"addformat", retvar, e.val))
    else:
      result.add(newCall(bindsym"addformat", retvar, e.val, e.fmt))
  for i, arg in argexprs:
    if arg.cnt == 0:
      warning("Argument " & $(i+1) & " `" & args[i].repr & "` is not used in format string")

macro fmt*(fmtstr: string{lit}; args: varargs[untyped]) : untyped =
  ## Formats arguments `args` according to the format string `fmtstr`.
  var retvar = gensym(nskVar, "ret")
  result = newNimNode(nnkStmtListExpr)
  result.add(newVarStmt(retvar, newCall(bindsym"newString", newLit(0))))
  result.add(addfmtfmt($fmtstr, args, retvar))
  result.add(retvar)

when not defined(js):
  macro writefmt*(f: File; fmtstr: string{lit}; args: varargs[untyped]): untyped =
    ## The same as `write(f, fmtstr.fmt(args...))` but faster.
    result = addfmtfmt($fmtstr, args, f)

  macro writelnfmt*(f: File; fmtstr: string{lit}; args: varargs[untyped]): untyped =
    ## The same as `writeln(f, fmtstr.fmt(args...))` but faster.
    result = addfmtfmt($fmtstr & "\n", args, f)

  macro printfmt*(fmtstr: string{lit}; args: varargs[untyped]): untyped =
    ## The same as `writefmt(stdout, fmtstr, args...)`.
    result = addfmtfmt($fmtstr, args, bindsym"stdout")

  macro printlnfmt*(fmtstr: string{lit}; args: varargs[untyped]): untyped =
    ## The same as `writelnfmt(stdout, fmtstr, args...)`.
    result = addfmtfmt($fmtstr & "\n", args, bindsym"stdout")

macro writefmt*(s: Stream; fmtstr: string{lit}; args: varargs[untyped]): untyped =
  ## The same as `write(s, fmtstr.fmt(args...))` but faster.
  result = addfmtfmt($fmtstr, args, s)

macro writelnfmt*(s: Stream; fmtstr: string{lit}; args: varargs[untyped]): untyped =
  ## The same as `writeln(s, fmtstr.fmt(args...))` but faster.
  result = addfmtfmt($fmtstr & "\n", args, s)

macro addfmt*(s: var string, fmtstr: string{lit}, args: varargs[untyped]): untyped =
  ## The same as `s.add(fmtstr.fmt(args...))` but faster.
  result = addfmtfmt($fmtstr, args, s)

proc geninterp(fmtstr: string): NimNode {.compileTime.} =
  ## Generate `fmt` expression for interpolated string `fmtstr`.
  var pos = 0
  var fstr = ""
  var args = newseq[NimNode]()
  var loop = true
  while loop and pos < fmtstr.len:
    let n = fmtstr.skipUntil({'$', '{', '}'}, pos)
    fstr.add(fmtstr.substr(pos, pos+n-1))
    pos += n
    if pos >= fmtstr.len:
      loop = false
    elif fmtstr[pos] == '{':
      pos += 1
      fstr.add("{{")
    elif fmtstr[pos] == '}':
      pos += 1
      fstr.add("}}")
    else: # fmtstr[pos] == '$'
      pos += 1
      if fmtstr[pos] == '$':
        pos += 1
        fstr.add('$')
      elif fmtstr[pos] in IdentStartChars:
        var ident: string
        let beg = pos
        pos += fmtstr.parseIdent(ident, pos)
        # parse the identifier
        try:
          args.add(parseExpr(ident))
        except ValueError:
          error "Cannot parse identifier after $ at character " & $beg
        fstr.add("{}")
      elif fmtstr[pos] == '{':
        pos += 1
        let beg = pos
        # parse the expression
        while true:
          pos += fmtstr.skipUntil({':', '}'}, pos)
          try:
            let e = parseExpr(fmtstr.substr(beg, pos-1))
            args.add(e)
            break
          except ValueError:
            if pos >= fmtstr.len:
              error "Cannot parse ${...} expression starting at character " & $beg
            pos += 1
        if fmtstr[pos] == ':':
          var fmtarg: string
          pos += fmtstr.parseUntil(fmtarg, '}', pos)
          pos += 1
          fstr.add("{")
          fstr.add(fmtarg)
          fstr.add("}")
        else:
          pos += 1
          fstr.add("{}")
      else:
        error("Invalid format string: expected $ or { after $")
  result = newCall(bindsym"fmt", fstr.newLit)
  for arg in args:
    result.add(arg)

macro interp*(fmtstr: string{lit}): untyped =
  ## Interpolate `fmtstr` with expressions.
  result = geninterp(fmtstr.strval)

macro `$$`*(fmtstr: string{lit}): untyped =
  ## Interpolate `fmtstr` with expressions.
  result = geninterp(fmtstr.strval)

when isMainModule:
  import unittest

  suite "Strings":
    test "string with 's' -- format string length = 0":
      check:
        "hello".format() == "hello"
        "hello".format("") == "hello"

    test "string with 's' -- format string length = 1":
      check:
        "hello".format("s") == "hello"

    test "string with 's' -- format string length >= 2":
      check:
        "hi".format("0s") == "hi"
        "hi".format("1s") == "hi"
        "hi".format("2s") == "hi"
        "hi".format("3s") == "hi "
        "hi".format("4s") == "hi  "
        "hello".format("10s") == "hello     "
        "hello".format("<10s") == "hello     "
        "hello".format(">10s") == "     hello"
        "hello".format("^10s") == "  hello   "
        "hello".format("^11s") == "   hello   "
        "hello".format(".<10s") == "hello....."
        "hello".format("ä>10s") == "ääääähello"
        "hello".format(".^10s") == "..hello..."
        "hello".format(".^11s") == "...hello..."

    test "string without 's'":
      check:
        "hällobello".format("") == "hällobello"
        "hällo".format("") == "hällo"
        "hällo".format("10") == "hällo     "
        "hällo".format("<10") == "hällo     "
        "hällo".format(">10") == "     hällo"
        "hällo".format("^10") == "  hällo   "
        "hällo".format("^11") == "   hällo   "
        "hällo".format(".<10") == "hällo....."
        "hällo".format(".>10") == ".....hällo"
        "hällo".format(".^10") == "..hällo..."
        "hällo".format("ü^11") == "üüühälloüüü"

    test "string truncation using X.Y format":
      check:
        "123456".format("10.3") == "123       "


  suite "Integers":
    test "positive integers -- format string length = 1":
      check:
        42.format("8") == "      42"

    test "positive integers":
      check:
        42.format() == "42"
        42.format("") == "42"
        42.format("<8") == "42      "
        42.format(">8") == "      42"
        42.format("^8") == "   42   "
        42.format("=8") == "      42"

        42.format(".<8") == "42......"
        42.format(".>8") == "......42"
        42.format(".^8") == "...42..."
        42.format(".=8") == "......42"
        42.format(".< 8") == " 42....."
        42.format(".> 8") == "..... 42"
        42.format(".^ 8") == ".. 42..."
        42.format(".= 8") == " .....42"
        42.format(".<+8") == "+42....."
        42.format(".>+8") == ".....+42"
        42.format(".^+8") == "..+42..."
        42.format(".=+8") == "+.....42"
        42.format(".<-8") == "42......"
        42.format("0>-8") == "00000042"
        42.format(".^-8") == "...42..."
        42.format("-08") == "00000042"

    test "positive integers with sign prefix":
      check:
        0.format("+") == "+0"
        123.format("+") == "+123"

    test "positive integers with commas":
      check:
        999.format(",") == "999"
        1000.format(",") == "1,000"
        123456.format(",") == "123,456"
        1234567.format(",") == "1,234,567"

        1234567.format("7,") == "1,234,567"
        1234567.format("10,") == " 1,234,567"
        1234567.format("011,") == "001,234,567"

    test "negative integers":
      check:
        (-42).format(".<8") == "-42....."
        (-42).format(".>8") == ".....-42"
        (-42).format(".^8") == "..-42..."
        (-42).format(".=8") == "-.....42"
        (-42).format(".< 8") == "-42....."
        (-42).format(".> 8") == ".....-42"
        (-42).format(".^ 8") == "..-42..."
        (-42).format(".= 8") == "-.....42"
        (-42).format(".<+8") == "-42....."
        (-42).format(".>+8") == ".....-42"
        (-42).format(".^+8") == "..-42..."
        (-42).format(".=+8") == "-.....42"
        (-42).format(".<-8") == "-42....."
        (-42).format(".>-8") == ".....-42"
        (-42).format(".^-8") == "..-42..."
        (-42).format("0=-8") == "-0000042"
        (-42).format("-08") == "-0000042"

    test "hex -- format string length = 1":
      check:
        0x1f5.format("x") == "1f5"
        0x1f5.format("X") == "1F5"

    test "hex -- format string length >= 2":
      check:
        0x1f5.format("#x") == "0x1f5"
        0x1f5.format("#X") == "0x1F5"

    test "binary -- format string length = 1":
      check:
        42.format("b") == "101010"

    test "binary -- format string length >= 2":
      check:
        42.format("#b") == "0b101010"

    test "octal -- format string length = 1":
      check:
        0x1f5.format("o") == "765"

    test "octal -- format string length >= 2":
      check:
        0x1f5.format("#o") == "0o765"


  suite "Characters":
    test "char -- format string length = 1":
      check:
        'a'.format("c") == "a"

    test "char -- format string length >= 2":
      check:
        'a'.format("6c") == "a     "
        'a'.format("<6c") == "a     "
        'a'.format(">6c") == "     a"
        'a'.format("^6c") == "  a   "
        'a'.format("^7c") == "   a   "
        'a'.format(".<6c") == "a....."
        'a'.format("ä>6c") == "äääääa"
        'a'.format(".^6c") == "..a..."
        'a'.format(".^7c") == "...a..."


  suite "Runes":
    test "rune -- format string length = 1":
      check:
        "ß".runeAt(0).format("c") == "ß"

    test "rune -- format string length >= 2":
      check:
        "ß".runeAt(0).format("6c") == "ß     "
        "ß".runeAt(0).format("<6c") == "ß     "
        "ß".runeAt(0).format(">6c") == "     ß"
        "ß".runeAt(0).format("^6c") == "  ß   "
        "ß".runeAt(0).format("^7c") == "   ß   "
        "ß".runeAt(0).format(".<6c") == "ß....."
        "ß".runeAt(0).format("ä>6c") == "äääääß"
        "ß".runeAt(0).format(".^6c") == "..ß..."
        "ß".runeAt(0).format(".^7c") == "...ß..."


  suite "Floats":
    setup:
      const
        zero = 0.0
        negZero = zero * -1

    test "positive infinity -- format string length = 0":
      check:
        Inf.format("") == "inf"

    test "positive infinity -- format string length = 1":
      check:
        Inf.format("8") == "     inf"

    test "positive infinity -- format string length >= 2":
      check:
        Inf.format("<8") == "inf     "
        Inf.format(">8") == "     inf"
        Inf.format("^8") == "  inf   "
        Inf.format("=8") == "     inf"

        Inf.format(".<8") == "inf....."
        Inf.format(".>8") == ".....inf"
        Inf.format(".^8") == "..inf..."
        Inf.format(".=8") == ".....inf"
        Inf.format(".< 8") == " inf...."
        Inf.format(".> 8") == ".... inf"
        Inf.format(".^ 8") == ".. inf.."
        Inf.format(".= 8") == " ....inf"
        Inf.format(".<+8") == "+inf...."
        Inf.format(".>+8") == "....+inf"
        Inf.format(".^+8") == "..+inf.."
        Inf.format(".=+8") == "+....inf"
        Inf.format(".<-8") == "inf....."
        Inf.format("0>-8") == "00000inf"
        Inf.format(".^-8") == "..inf..."
        Inf.format("0=-8") == "00000inf"
        Inf.format("-08") == "00000inf"

    test "negative infinity -- format string length = 0":
      check:
        NegInf.format("") == "-inf"

    test "negative infinity -- format string length = 1":
      check:
        NegInf.format("8") == "    -inf"

    test "negative infinity -- format string length >= 2":
      check:
        NegInf.format("<8") == "-inf    "
        NegInf.format(">8") == "    -inf"
        NegInf.format("^8") == "  -inf  "
        NegInf.format("=8") == "-    inf"

        NegInf.format(".<8") == "-inf...."
        NegInf.format(".>8") == "....-inf"
        NegInf.format(".^8") == "..-inf.."
        NegInf.format(".=8") == "-....inf"
        NegInf.format(".< 8") == "-inf...."
        NegInf.format(".> 8") == "....-inf"
        NegInf.format(".^ 8") == "..-inf.."
        NegInf.format(".= 8") == "-....inf"
        NegInf.format(".<+8") == "-inf...."
        NegInf.format(".>+8") == "....-inf"
        NegInf.format(".^+8") == "..-inf.."
        NegInf.format(".=+8") == "-....inf"
        NegInf.format(".<-8") == "-inf...."
        NegInf.format("0>-8") == "0000-inf"
        NegInf.format(".^-8") == "..-inf.."
        NegInf.format("0=-8") == "-0000inf"
        NegInf.format("-08") == "-0000inf"

    test "positive 0.0":
      check:
        0.0.format(".<8") == "0......."
        0.0.format(".>8") == ".......0"
        0.0.format(".^8") == "...0...."
        0.0.format(".=8") == ".......0"
        0.0.format(".< 8") == " 0......"
        0.0.format(".> 8") == "...... 0"
        0.0.format(".^ 8") == "... 0..."
        0.0.format(".= 8") == " ......0"
        0.0.format(".<+8") == "+0......"
        0.0.format(".>+8") == "......+0"
        0.0.format(".^+8") == "...+0..."
        0.0.format(".=+8") == "+......0"
        0.0.format(".<-8") == "0......."
        0.0.format("0>-8") == "00000000"
        0.0.format(".^-8") == "...0...."
        0.0.format("0=-8") == "00000000"
        0.0.format("-08") == "00000000"
        zero.format("5.2e") == "0.00e+00"

    test "negative 0.0 -- format string length = 0":
      check:
        negZero.format("") == "-0"

    test "negative 0.0 -- format string length = 1":
      check:
        negZero.format("8") == "      -0"

    test "negative 0.0 -- format string length >= 2":
      check:
        negZero.format("<8") == "-0      "
        negZero.format(">8") == "      -0"
        negZero.format("^8") == "   -0   "
        negZero.format("=8") == "-      0"

        negZero.format(".<8") == "-0......"
        negZero.format(".>8") == "......-0"
        negZero.format(".^8") == "...-0..."
        negZero.format(".=8") == "-......0"
        negZero.format(".< 8") == "-0......"
        negZero.format(".> 8") == "......-0"
        negZero.format(".^ 8") == "...-0..."
        negZero.format(".= 8") == "-......0"
        negZero.format(".<+8") == "-0......"
        negZero.format(".>+8") == "......-0"
        negZero.format(".^+8") == "...-0..."
        negZero.format(".=+8") == "-......0"
        negZero.format(".<-8") == "-0......"
        negZero.format("0>-8") == "000000-0"
        negZero.format(".^-8") == "...-0..."
        negZero.format("0=-8") == "-0000000"
        negZero.format("-08") == "-0000000"

        negZero.format("5.2e") == "-0.00e+00"

    test "more floats -- format string length = 0":
      check:
        123.456.format() == "123.456"
        123.45678.format() == "123.457"

    test "more floats -- format string length = 1":
      check:
        123.456.format("f") == "123.456000"
        123.456.format("e") == "1.234560e+02"
        123.456.format("E") == "1.234560E+02"
        1.0.format("e") == "1.000000e+00"
        0.00123456.format("f") == "0.001235"
        0.00123456.format("e") == "1.234560e-03"
        0.00123456.format("g") == "0.00123456"
        0.000123456.format("g") == "0.000123456"
        0.0000123456.format("g") == "1.23456e-05"
        0.3.format("%") == "30.000000%"

    test "more floats -- format string length >= 2":
      check:
        123.456.format(".2f") == "123.46"
        123.456.format("8.2f") == "  123.46"
        123.456.format(".2e") == "1.23e+02"
        123.456.format(".<10.2e") == "1.23e+02.."
        123.456.format(".2g") == "1.2e+02"
        123.456.format(".2G") == "1.2E+02"
        123.456.format(".3g") == "123"
        123.456.format(".10g") == "123.456"
        1234.56.format(".10g") == "1234.56"
        1234.56.format(",.10g") == "1,234.56"
        123456.789.format(",.10g") == "123,456.789"
        1234567.89.format(",.10g") == "1,234,567.89"
        1234567.89.format(",.3f") == "1,234,567.890"
        1234567.89.format("15,.3f") == "  1,234,567.890"
        0.00123456.format(".4g") == "0.001235"
        0.00123456.format(".1g") == "0.001"
        0.0000123456.format(".3g") == "1.23e-05"
        0.0000123456.format("0=+10.3g") == "+01.23e-05"
        0.0000123456.format("0= 10.3g") == " 01.23e-05"
        (-0.0000123456).format("0=10.3") == "-01.23e-05"
        0.3.format(".2%") == "30.00%"
        0.5.format("0.2f") == "0.50"


  suite "Booleans":
    test "booleans -- format string length = 0":
      check:
        true.format("") == "true"
        false.format("") == "false"

    test "booleans -- format string length = 1":
      check:
        true.format("s") == "true"
        false.format("s") == "false"
        true.format("d") == "1"
        false.format("d") == "0"


  suite "Arrays":
    test "arrays of arrays":
      check:
        [[1,2,3], [4,5,6]].format("3a:;\n :, ") == "  1,   2,   3;\n   4,   5,   6"
        [[1,2,3], [4,5,6]].format("") == "1\t2\t3\t4\t5\t6"
        [[1.0,2.0,3.0], [4.0,5.0,6.0]].format(".1e") == "1.0e+00\t2.0e+00\t3.0e+00\t4.0e+00\t5.0e+00\t6.0e+00"


  suite "fmt syntax":
    setup:
      let
        x = 32
        y = 8
      var
        z = 0
        s = ""

    test "fmt":
      check:
        ("number: {} with width: {:5.2f} string: {:.^9} array: {:a:, } end".fmt(42, 1.45, "hello", [1,2,3]) ==
             "number: 42 with width:  1.45 string: ..hello.. array: 1, 2, 3 end")
        "{{{}}}".fmt("hallo") == "{hallo}"

        "[{:{}^{}x}]".fmt(66, '.', 6) == "[..42..]"
        "[{:{}{}{}}]{{{:{}{}{}}}}".fmt(5, '.', '>', 6, "abc", "-", "^", 10) == "[.....5]{---abc----}"
        "[{0:{1}{2}{3}}]".fmt(5, '.', '>', 6) == "[.....5]"
        "[{3:{1}{2}{3}}]:{0}".fmt(5, '.', '>', 6) == "[.....6]:5"
        "[{3:{2}{2}{3}}]:{0}{1}".fmt(5, '.', '>', 6) == "[>>>>>6]:5."

        "[{0:{1}{2}{3}}]".fmt(5, '.', '>', 6) == "[.....5]"

        "{0.name:.^10} {0.age}".fmt((name:"john", age:27)) == "...john... 27"
        "{0[1]:.^10} {0[0]}".fmt(["27", "john"]) == "...john... 27"

        "{} {}".fmt(125u8, 254u8) == "125 254"


    test "fmt of floats with exponentiation -- issue #2":
      # https://github.com/kaushalmodi/strfmt/issues/2
      check:
        "{:f}".fmt(9.223372036854776e+18) == "9.223372036854776e+18"
      skip

    test "proc inside fmt":
      check:
        "{0} {0}".fmt((z.inc; z)) == "1 1"

    test "$$":
      check:
        $$"interpolate ${32} == ${4.2}" == "interpolate 32 == 4.2"
        $$"${x} + ${y} == ${x + y}" == "32 + 8 == 40"
        $$"max($x, $y) == ${max(x,y)}" == "max(32, 8) == 32"
        interp"formatted: ${4:.^5}" == "formatted: ..4.."
        # (interp"formatted: ${{1:2,3:4}[1][0]}" == "formatted: 3")
        # (interp"formatted: ${{1:2,3:4}[1][0]:.^10}" == "formatted: ....3.....")
        # (interp"formatted: ${if true: 1 else: 2}" == "formatted: 1")
        # ($$"formatted: ${if true: \"a\" else: \"b\"}" == "formatted: a")

    test "addfmt":
      check:
        (s.addfmt("a:{}", 42); s) == "a:42"
