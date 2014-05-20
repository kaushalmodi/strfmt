import macros
import strutils
import unicode
import pegs

type
  EFormat = object of EBase

  TWrite*[T] = proc (o: var T; c: char)

  TFormat* = tuple
    fill: string  ## the fill character, UTF8
    align: char   ## the alignment, either <, >, ^, = or \0 (default)
    sign: char    ## the sign, either +, - or SPACE (- default)
    width: int
    comma: bool
    precision: int
    typ: char     ## the format type: bcdeEfFgGnosxX%

proc parse*(fmt: string): TFormat =
  let p=peg"{(_&[<>=^])?}{[<>=^]?}{[-+ ]?}{[0-9]+?}{[,]?}{([.][0-9]+)?}{[bcdeEfFgGnosxX%]?}"
  var m : array[1..7, string]
  if not fmt.match(p, m):
    raise newException(EFormat, "Invalid format string")

  result.fill  = if m[1].len > 0: m[1] else: " "
  result.align = if m[2].len > 0: m[2][0] else: 0.char
  result.sign  = if m[3].len > 0: m[3][0] else: '-'

  if m[4].len > 0:
    result.width = m[4].parseInt
  else:
    result.width = -1

  result.comma = m[5].len == 1

  if m[6].len > 0:
    result.precision = m[6].substr(1).parseInt

  result.typ = if m[7].len > 0: m[7][0] else: 0.char

proc getalign(fmt: TFormat; defalg: char; slen: int) : tuple[left, right:int] =
  result.left = 0
  result.right = 0
  if (fmt.width >= 0) and (slen < fmt.width):
    let alg = if fmt.align == 0.char: defalg else: fmt.align
    case alg:
    of '<': result.right = fmt.width - slen
    of '>', '=': result.left = fmt.width - slen
    of '^':
      result.left = (fmt.width - slen) div 2
      result.right = fmt.width - slen - result.left
    else: discard

proc writefill[Obj](o: var Obj; add: TWrite[Obj]; fmt: TFormat; n: int) =
  for i in 1..n:
    for c in fmt.fill.items:
      add(o, c)

proc writef*[Obj](o: var Obj; add: TWrite[Obj]; s: string; fmt: TFormat) =
  if not (fmt.typ in {'s', 0.char}):
    raise newException(EFormat, "String variable must have 's' format type")

  # compute alignment
  var alg = getalign(fmt, '<', runelen(s))
  writefill(o, add, fmt, alg.left)
  for c in s.items: add(o, c)
  writefill(o, add, fmt, alg.right)

proc writef*[Obj](o: var Obj; add: TWrite[Obj]; i: TInteger; fmt: TFormat) =
  if not (fmt.typ in {0.char, 'b', 'o', 'x', 'X', 'd', 'n'}):
    raise newException(EFormat, "Integer variable must of one of the following types: b,o,x,X,d,n")

  var base : type(i) = 10
  case fmt.typ:
  of 'b': base = 2
  of 'o': base = 8
  of 'x', 'X': base = 16
  else: discard

  var len = 0
  if fmt.sign != '-' or i < 0: len.inc

  var x : type(i) = abs(i)
  var irev : type(i) = 0
  var ilen = 0
  while x > 0:
    len.inc
    ilen.inc
    irev = irev * base + x mod base
    x = x div base

  var alg = getalign(fmt, '>', len)


  if fmt.align != '=':
    writefill(o, add, fmt, alg.left)

  if i < 0: add(o, '-')
  elif fmt.sign == '+': add(o, '+')
  elif fmt.sign == ' ': add(o, ' ')

  if fmt.align == '=':
    writefill(o, add, fmt, alg.left)

  while ilen > 0:
    ilen.dec
    let c = irev mod base
    irev = irev div base
    if c < 10:
      add(o, ('0'.int + c).char)
    elif fmt.typ == 'x':
      add(o, ('a'.int + c - 10).char)
    else:
      add(o, ('A'.int + c - 10).char)
  writefill(o, add, fmt, alg.right)

proc writef*[Obj,T](o: var Obj; add: TWrite[Obj]; x: T; fmt: semistatic[string]) {.inline.} =
  when isstatic(fmt):
    var f {.global.} = fmt.parse
    writef(o, add, x, f)
  else:
    writef(o, add, x, fmt.parse)

proc format*[T](x: T; fmt: TFormat): string =
  result = ""
  writef(result, proc (o: var string; c: char) = o.add(c), x, fmt)

proc format*[T](x: T; fmt: string): string =
  result = format(x, fmt.parse)
  # when isstatic(fmt):
  #   var f {.global.} = fmt.parse
  #   result = format(x, f)
  # else:
  #   result = format(x, fmt.parse)

macro fmt*(s: string; args: varargs[expr]) : expr =
  result = newStmtList(newLit"")
  var pos = 0
  var arg = 0
  while true:
    let opbeg = find($s, "{", pos)
    if opbeg < 0: break

    let opend = find($s, "}", opbeg+1)
    if opend < 0: quit "Invalid format string: unclosed {"
    if opbeg > pos:
      result = infix(result, "&", newLit(substr($s, pos, opbeg-1)))
    result = infix(result, "&", newCall("format".ident,
                                        args[arg],
                                        newLit(substr($s, opbeg+1, opend-1))))
    arg += 1
    pos = opend+1
  if pos < len($s):
    result = infix(result, "&", newLit(substr($s, pos)))



when isMainModule:    
  # string with 's'
  doassert "hello".format("s") == "hello"
  doassert "hello".format("10s") == "hello     "
  doassert "hello".format("<10s") == "hello     "
  doassert "hello".format(">10s") == "     hello"
  doassert "hello".format("^10s") == "  hello   "
  doassert "hello".format("^11s") == "   hello   "
  doassert "hello".format(".<10s") == "hello....."
  doassert "hello".format("ä>10s") == "ääääähello"
  doassert "hello".format(".^10s") == "..hello..."
  doassert "hello".format(".^11s") == "...hello..."

  # string without 's'
  doassert "hällo".format("") == "hällo"
  doassert "hällo".format("10") == "hällo     "
  doassert "hällo".format("<10") == "hällo     "
  doassert "hällo".format(">10") == "     hällo"
  doassert "hällo".format("^10") == "  hällo   "
  doassert "hällo".format("^11") == "   hällo   "
  doassert "hällo".format(".<10") == "hällo....."
  doassert "hällo".format(".>10") == ".....hällo"
  doassert "hällo".format(".^10") == "..hällo..."
  doassert "hällo".format("ü^11") == "üüühälloüüü"

  # integer
  doassert 42.format("") == "42"
  doassert 42.format("8") == "      42"
  doassert 42.format("<8") == "42      "
  doassert 42.format(">8") == "      42"
  doassert 42.format("^8") == "   42   "
  doassert 42.format("=8") == "      42"

  doassert 42.format(".<8") == "42......"
  doassert 42.format(".>8") == "......42"
  doassert 42.format(".^8") == "...42..."
  doassert 42.format(".=8") == "......42"
  doassert 42.format(".< 8") == " 42....."
  doassert 42.format(".> 8") == "..... 42"
  doassert 42.format(".^ 8") == ".. 42..."
  doassert 42.format(".= 8") == " .....42"
  doassert 42.format(".<+8") == "+42....."
  doassert 42.format(".>+8") == ".....+42"
  doassert 42.format(".^+8") == "..+42..."
  doassert 42.format(".=+8") == "+.....42"
  doassert 42.format(".<-8") == "42......"
  doassert 42.format("0>-8") == "00000042"
  doassert 42.format(".^-8") == "...42..."
  doassert 42.format("0=-8") == "00000042"

  doassert((-42).format(".<8") == "-42.....")
  doassert((-42).format(".>8") == ".....-42")
  doassert((-42).format(".^8") == "..-42...")
  doassert((-42).format(".=8") == "-.....42")
  doassert((-42).format(".< 8") == "-42.....")
  doassert((-42).format(".> 8") == ".....-42")
  doassert((-42).format(".^ 8") == "..-42...")
  doassert((-42).format(".= 8") == "-.....42")
  doassert((-42).format(".<+8") == "-42.....")
  doassert((-42).format(".>+8") == ".....-42")
  doassert((-42).format(".^+8") == "..-42...")
  doassert((-42).format(".=+8") == "-.....42")
  doassert((-42).format(".<-8") == "-42.....")
  doassert((-42).format(".>-8") == ".....-42")
  doassert((-42).format(".^-8") == "..-42...")
  doassert((-42).format("0=-8") == "-0000042")

  doassert 0x1f5.format("x") == "1f5"
  doassert 0x1f5.format("X") == "1F5"
  doassert 0x1f5.format("o") == "765"
  doassert 42.format("b") == "101010"
