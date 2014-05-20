import macros
import strutils
import unicode
import pegs
import math

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

const
  DefaultPrec = 6
  round_nums = [0.5, 0.05, 0.005, 0.0005, 0.00005, 0.000005, 0.0000005, 0.00000005]

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
  else:
    result.precision = -1

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

proc writefill[Obj](o: var Obj; add: TWrite[Obj]; fmt: TFormat; n: int; sign: int = 0) =

  if fmt.align == '=' and sign != 0:
    if sign < 0: add(o, '-')
    elif fmt.sign == '+': add(o, '+')
    elif fmt.sign == ' ': add(o, ' ')

  for i in 1..n:
    for c in fmt.fill.items:
      add(o, c)

  if fmt.align != '=' and sign != 0:
    if sign < 0: add(o, '-')
    elif fmt.sign == '+': add(o, '+')
    elif fmt.sign == ' ': add(o, ' ')

proc writef*[Obj](o: var Obj; add: TWrite[Obj]; s: string; fmt: TFormat) =
  if not (fmt.typ in {'s', 0.char}):
    raise newException(EFormat, "String variable must have 's' format type")

  # compute alignment
  var alg = getalign(fmt, '<', runelen(s))
  writefill(o, add, fmt, alg.left)
  for c in s.items: add(o, c)
  writefill(o, add, fmt, alg.right)

proc writef*[Obj](o: var Obj; add: TWrite[Obj]; c: char; fmt: TFormat) =
  if not (fmt.typ in {'c', 0.char}):
    raise newException(EFormat, "Character variable must have 'c' format type")

  # compute alignment
  var alg = getalign(fmt, '<', 1)
  writefill(o, add, fmt, alg.left)
  add(o, c)
  writefill(o, add, fmt, alg.right)

proc writef*[Obj](o: var Obj; add: TWrite[Obj]; c: TRune; fmt: TFormat) =
  if not (fmt.typ in {'c', 0.char}):
    raise newException(EFormat, "Character variable must have 'c' format type")

  # compute alignment
  var alg = getalign(fmt, '<', 1)
  writefill(o, add, fmt, alg.left)
  let s = c.toUTF8
  for c in s: add(o, c)
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


  writefill(o, add, fmt, alg.left, i)
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

proc writeSpecialFloat[Obj](o: var Obj; add: TWrite[Obj]; x: TReal; fmt: TFormat) =
  ## Format special floating point numbers nan, 0, -0, inf and -inf
  var len = 0
  case x.classify
  of fcNaN:
    len = 3
  of fcInf:
    len = if fmt.sign != '-': 4 else: 3
  of fcNegInf:
    len = 4
  of fcZero:
    len = if fmt.sign != '-': 2 else: 1
  of fcNegZero:
    len = 2
  else:
    discard

  let alg = getalign(fmt, '>', len)

  writefill(o, add, fmt, alg.left, if x.classify in {fcNegInf, fcNegZero}: -1 else: +1)
  case x.classify
  of fcNaN:
    for c in "nan": add(o, c)
  of fcInf, fcNegInf:
    for c in "inf": add(o, c)
  of fcZero, fcNegZero:
    add(o, '0')
  else:
    discard
  writefill(o, add, fmt, alg.right)

proc writef*[Obj](o: var Obj; add: TWrite[Obj]; x: TReal; fmt: TFormat) =
  if not (fmt.typ in {0.char, 'e', 'E', 'f', 'F', 'g', 'G', 'n', '%'}):
    raise newException(EFormat, "Integer variable must of one of the following types: b,o,x,X,d,n")

  var len = 0
  if x.classify in {fcNan, fcInf, fcNegInf, fcZero, fcNegZero}:
    writeSpecialFloat(o, add, x, fmt); return

  if fmt.sign != '-' or x < 0: len.inc

  var prec = if fmt.precision < 0: DefaultPrec else: fmt.precision
  var y = abs(x)
  var exp = 0
  var numstr, frstr: array[0..31, char]
  var numlen, frbeg, frlen = 0

  if fmt.typ == '%': y *= 100

  case x.classify:
  of fcNan:
    numstr[0..2] = ['n', 'a', 'n']
    numlen = 3
  of fcInf, fcNegInf:
    numstr[0..2] = ['i', 'n', 'f']
    numlen = 3
  of fcZero, fcNegZero:
    numstr[0] = '0'
    numlen = 1
  else: # a usual fractional number
    if not (fmt.typ in {'f', 'F', '%'}): # not fixed point
      exp = log10(y).floor.int
      if fmt.typ in {'g', 'G', 0.char}:
        if -4 <= exp and exp < prec:
          prec = prec-1-exp
          exp = 0
        else:
          prec = prec - 1
          len += 4 # exponent
      else:
        len += 4 # exponent
      # shift y so that 1 <= abs(y) < 2
      var mult = 1
      for i in 1..abs(exp): mult *= 10
      if exp > 0: y /= mult.TReal
      elif exp < 0: y *= mult.TReal
    elif fmt.typ == '%':
      len += 1 # percent sign

    # handle rounding by adding +0.5 * LSB
    if prec < round_nums.len: y += round_nums[prec]

    # split into integer and fractional part
    var mult = 1'i64
    for i in 1..prec: mult *= 10
    var num = y.int64
    var fr = ((y - num.TReal) * mult.TReal).int64
    # build integer part string
    while num != 0:
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
    if fmt.typ in {'g', 'G'}:
      while frbeg < frlen and frstr[frbeg] == '0': frbeg.inc
    # update length of string
    len += numlen;
    if frbeg < frlen:
      len += 1 + frlen - frbeg # decimal point and fractional string

  let alg = getalign(fmt, '>', len)
  writefill(o, add, fmt, alg.left, if x > 0: 1 else: -1)
  for i in (numlen-1).countdown(0): add(o, numstr[i])
  if frbeg < frlen:
    add(o, '.')
    for i in (frlen-1).countdown(frbeg): add(o, frstr[i])
  if fmt.typ in {'e', 'E'} or (fmt.typ in {'g', 'G', 0.char} and exp != 0):
    add(o, if fmt.typ in {'e', 'g', 0.char}: 'e' else: 'E')
    writef(o, add, exp, "0=+3")
  if fmt.typ == '%': add(o, '%')
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

  doassert 'a'.format("c") == "a"
  doassert 'a'.format("6c") == "a     "
  doassert 'a'.format("<6c") == "a     "
  doassert 'a'.format(">6c") == "     a"
  doassert 'a'.format("^6c") == "  a   "
  doassert 'a'.format("^7c") == "   a   "
  doassert 'a'.format(".<6c") == "a....."
  doassert 'a'.format("ä>6c") == "äääääa"
  doassert 'a'.format(".^6c") == "..a..."
  doassert 'a'.format(".^7c") == "...a..."

  doassert "ß".runeat(0).format("c") == "ß"
  doassert "ß".runeat(0).format("6c") == "ß     "
  doassert "ß".runeat(0).format("<6c") == "ß     "
  doassert "ß".runeat(0).format(">6c") == "     ß"
  doassert "ß".runeat(0).format("^6c") == "  ß   "
  doassert "ß".runeat(0).format("^7c") == "   ß   "
  doassert "ß".runeat(0).format(".<6c") == "ß....."
  doassert "ß".runeat(0).format("ä>6c") == "äääääß"
  doassert "ß".runeat(0).format(".^6c") == "..ß..."
  doassert "ß".runeat(0).format(".^7c") == "...ß..."

  doassert Inf.format("") == "inf"
  doassert Inf.format("8") == "     inf"
  doassert Inf.format("<8") == "inf     "
  doassert Inf.format(">8") == "     inf"
  doassert Inf.format("^8") == "  inf   "
  doassert Inf.format("=8") == "     inf"

  doassert Inf.format(".<8") == "inf....."
  doassert Inf.format(".>8") == ".....inf"
  doassert Inf.format(".^8") == "..inf..."
  doassert Inf.format(".=8") == ".....inf"
  doassert Inf.format(".< 8") == " inf...."
  doassert Inf.format(".> 8") == ".... inf"
  doassert Inf.format(".^ 8") == ".. inf.."
  doassert Inf.format(".= 8") == " ....inf"
  doassert Inf.format(".<+8") == "+inf...."
  doassert Inf.format(".>+8") == "....+inf"
  doassert Inf.format(".^+8") == "..+inf.."
  doassert Inf.format(".=+8") == "+....inf"
  doassert Inf.format(".<-8") == "inf....."
  doassert Inf.format("0>-8") == "00000inf"
  doassert Inf.format(".^-8") == "..inf..."
  doassert Inf.format("0=-8") == "00000inf"

  doassert NegInf.format("") == "-inf"
  doassert NegInf.format("8") == "    -inf"
  doassert NegInf.format("<8") == "-inf    "
  doassert NegInf.format(">8") == "    -inf"
  doassert NegInf.format("^8") == "  -inf  "
  doassert NegInf.format("=8") == "-    inf"

  doassert NegInf.format(".<8") == "-inf...."
  doassert NegInf.format(".>8") == "....-inf"
  doassert NegInf.format(".^8") == "..-inf.."
  doassert NegInf.format(".=8") == "-....inf"
  doassert NegInf.format(".< 8") == "-inf...."
  doassert NegInf.format(".> 8") == "....-inf"
  doassert NegInf.format(".^ 8") == "..-inf.."
  doassert NegInf.format(".= 8") == "-....inf"
  doassert NegInf.format(".<+8") == "-inf...."
  doassert NegInf.format(".>+8") == "....-inf"
  doassert NegInf.format(".^+8") == "..-inf.."
  doassert NegInf.format(".=+8") == "-....inf"
  doassert NegInf.format(".<-8") == "-inf...."
  doassert NegInf.format("0>-8") == "0000-inf"
  doassert NegInf.format(".^-8") == "..-inf.."
  doassert NegInf.format("0=-8") == "-0000inf"

  doassert 0'f64.format("") == "0"
  doassert 0'f64.format("8") == "       0"
  doassert 0'f64.format("<8") == "0       "
  doassert 0'f64.format(">8") == "       0"
  doassert 0'f64.format("^8") == "   0    "
  doassert 0'f64.format("=8") == "       0"

  doassert 0'f64.format(".<8") == "0......."
  doassert 0'f64.format(".>8") == ".......0"
  doassert 0'f64.format(".^8") == "...0...."
  doassert 0'f64.format(".=8") == ".......0"
  doassert 0'f64.format(".< 8") == " 0......"
  doassert 0'f64.format(".> 8") == "...... 0"
  doassert 0'f64.format(".^ 8") == "... 0..."
  doassert 0'f64.format(".= 8") == " ......0"
  doassert 0'f64.format(".<+8") == "+0......"
  doassert 0'f64.format(".>+8") == "......+0"
  doassert 0'f64.format(".^+8") == "...+0..."
  doassert 0'f64.format(".=+8") == "+......0"
  doassert 0'f64.format(".<-8") == "0......."
  doassert 0'f64.format("_>-8") == "_______0"
  doassert 0'f64.format(".^-8") == "...0...."
  doassert 0'f64.format("_=-8") == "_______0"

  doassert 123.456.format("f") == "123.456000"
  doassert 123.456.format(".2f") == "123.46"
  doassert 123.456.format("8.2f") == "  123.46"
  doassert 123.456.format("e") == "1.234560e+02"
  doassert 123.456.format("E") == "1.234560E+02"
  doassert 123.456.format(".2e") == "1.23e+02"
  doassert 123.456.format(".<10.2e") == "1.23e+02.."
  doassert 123.456.format(".2g") == "1.2e+02"
  doassert 123.456.format(".2G") == "1.2E+02"
  doassert 123.456.format(".3g") == "123"
  doassert 123.456.format(".10g") == "123.456"
  doassert 0.00123456.format("f") == "0.001235"
  doassert 0.00123456.format("e") == "1.234560e-03"
  doassert 0.00123456.format("g") == "0.00123456"
  doassert 0.00123456.format(".4g") == "0.001235"
  doassert 0.00123456.format(".1g") == "0.001"
  doassert 0.000123456.format("g") == "0.000123456"
  doassert 0.0000123456.format("g") == "1.23456e-05"
  doassert 0.0000123456.format(".3g") == "1.23e-05"
  doassert 0.0000123456.format("0=+10.3g") == "+01.23e-05"
  doassert 0.0000123456.format("0= 10.3g") == " 01.23e-05"
  doassert((-0.0000123456).format("0=10.3") == "-01.23e-05")
  doassert 0.3.format("%") == "30.000000%"
  doassert 0.3.format(".2%") == "30.00%"
