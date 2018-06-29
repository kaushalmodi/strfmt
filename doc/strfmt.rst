The most important functions and macros provided are:

1. the *format* functions to render a single value as a string,
2. the *fmt* macro to construct a string containing several
   formatted values
3. the *writefmt* and *printfmt* family of macros to write a
   formatted string to a file and *stdout*, respectively
4. the *interp* and *$$* string **interpolation** macros to
   render expressions embedded in the string itself

These functions are described in the following sections.

This package is hosted on `bitbucket
<https://bitbucket.org/lyro/strfmt>`_. If you encounter any bugs or
have some feature requests, please use the `issue tracker
<https://bitbucket.org/lyro/strfmt/issues?status=new&status=open>`_.

Formatting a single value: *format*
-----------------------------------
The *format* function transforms a single value to a string
according to a given *format string*, e.g.

.. code-block:: nim

  42.23.format("06.1f")

The syntax of the format specification string is similar to
`Python's Format Specification Mini-Language
<https://docs.python.org/3.4/library/string.html#formatspec>`_.

The general form of a format specifier is

::

  format_spec ::= [[fill]align][sign][#][0][width][,][.precision][type][a[array_sep]]
  fill        ::= rune
  align       ::= "<" | ">" | "^" | "="
  sign        ::= "+" | "-" | " "
  width       ::= integer
  precision   ::= integer
  type        ::= "b" | "c" | "d" | "e" | "E" | "f" | "F" | "g" | "G" | "n" | "o" | "s" | "x" | "X" | "%"
  array_sep   ::= "" | (<level separating character> string )+

The meaning of the various fields is as follows.

**fill**
  this character (or rune) is used to for the additional characters
  to be written until the formatted string is at least *width*
  characters long. The fill character defaults to SPACE.

**align**
  Special character to specify alignment.

  ====== =========
  Option Meaning
  ------ ---------
  ``<``  Left alignment, additional characters are added to the
         right (default for string).
  ``>``  Right alignment, additional characters are added to the left.
  ``^``  Centered , the same amount of characters is added to the
         left and the right.
  ``=``  Padding. If a numeric value is printed with a sign, then
         additional characters are added after the sign. Otherwise
         it behaves like "``>``". This option is only available for
         numbers (default for numbers).
  ====== =========

**sign**
  The sign character is only used for numeric values.

  =======  =========
  Option   Meaning
  -------  ---------
  ``+``    All numbers (including positive ones) are preceded by a sign.
  ``-``    Only negative numbers are preceded by a sign.
  *SPACE*  Negative numbers are preceded by a sign, positive numbers are preceded by a space.
  =======  =========

**#**
  If this character is present then the integer values in the
  formats ``b``, ``o``, ``x`` and ``X`` are preceded by *0b*, *0o*
  or *0x*, respectively. In all other formats this character is
  ignored.

**width**
  The minimal width of the resulting string. The result string is
  padded with extra characters (according the *align* field) until
  at least *width* characters have been written.

**,**
  Add , as a thousands separator

**precision**
  The meaning of the precision field depends on the formatting
  type.

  ============================= =========
  Type                          Meaning
  ----------------------------- ---------
  ``s``                         The maximal number of characters written.
  ``f``, ``F``, ``e`` and ``E`` The number of digits after the decimal point.
  ``g``, ``G``                  The number of significant digits written (i.e. the
                                number of overall digits).
  ============================= ==========

  Note that in all cases the decimal point is printed if and only
  if there is at least one digit following the point.

  The *precision* field is ignored in all other cases.

**type**
  The formatting type. The valid types depend on the type of the
  value to be printed. For strings the following types are valid.

  ===== =================================================
  Type  Meaning
  ----- -------------------------------------------------
  ``s`` A string. This is the default format for strings.
  ===== =================================================

  The following types are valid for integers.

  ===== ===========================================================
  Type  Meaning
  ----- -----------------------------------------------------------
  ``d`` A decimal integer number. This is the default for integers.
  ``b`` A binary integer (base 2).
  ``o`` An octal integer (base 8).
  ``x`` A hexadecimal integer (base 16), all letters are lower case.
  ``X`` A hexadecimal integer (base 16), all letters are upper case.
  ``n`` The same as ``d``.
  ===== ===========================================================

  The following types are valid for real numbers.

  ===== ===========================================================
  Type  Meaning
  ----- -----------------------------------------------------------
  ``f`` Fixed point format.
  ``F`` The same as f.
  ``e`` Scientific format, exactly one digit before the decimal
        point. The exponent is written with a lower case 'e'. The
        exponent always has a sign as at least two digits.
  ``E`` The same as ``e`` but with an upper case 'E'.
  ``g`` General format. The number is written either in fixed point
        format or in scientific format depending on the precision
        and the exponent in scientific format.

        The exact rule is as follows. Suppose *exp* is the exponent
        in scientific format and *p* the desired precision. If *-4
        <= exp <= p-1* then the number is formatted in fixed point
        format ``f`` with precision *p-1-exp*. Otherwise the number
        if formatted in scientific format ``e`` with precision
        *p-1*. Trailing zeros are removed in all cases and the
        decimal point is removed as well if there are no remaining
        digits following it.
  ``G`` The same as ``g`` but works like ``E`` if scientific format
        is used.
  ``%`` The number if multiplied by 100, formatted in fixed point
        format ``f`` and followed by a percent sign.
  ===== ===========================================================

**array_sep**
  If an array is formatted, the format specifications above apply
  to each element of the array. The elements are printed in
  succession separated by a separator string. If the array is
  nested then this applies recursively.

  The *array_sep* field specifies the separator string for all
  levels of a nested array. The first character after the *a* is
  the level separator and works as separator between the string for
  successive levels. It is never used in the resulting string. All
  characters between two level separators are the separator between
  two elements of the respective array level. See `Array formatting`_
  below.

Array formatting
----------------
A format string may contain a separator string for formatting
arrays. Because arrays might be nested the separator field contains
the separator strings to be used between two successive elements of
each level. The strings for each level are separated (in the format
string itself) by a special separating character. This character is
the first character after the ``a`` in the format string. The
following example should make this clear:

.. code-block:: nim

  [[2, 3, 4], [5, 6, 7]].format("02da|; |, ")

This code returns the string *"02, 03, 04; 05, 06, 07"*. The
special character separating the strings of different levels is the
first character after the ``a``, i.e. the pipe character ``|`` in
this example. Following the first pipe character is the separator
string for the outer most level, *"; "*. This means that after
printing the first element of the outermost array the string *"; "*
is printed. After the second pipe character comes the separator
string for the second level, in this example *", "*. Between each
two elements of the second level the separator string *", "* is
printed. Because the elements of the second level array are
integers, the format string "02d" applies to all of them. Thus,
each number is printed with a leading 0. After the 4 has been
printed the complete first element of the outer array (namely in
array *[2, 3, 4]*) has been printed, so the separator string of the
outer level follows, in this case a semicolon and a space. Finally
the second array *[6, 7, 8]* is printed with the separator ", "
between each two elements.

A string containing formatted values: *fmt*
-------------------------------------------
The *fmt* macro allows to interpolate a string with several
formatted values. This macro takes a format string as its first
argument and the values to be formatted in the remaining arguments.
The result is a formatted string expression. Note that the format
string *must* be a literal string.

A format string contains a replacement field within
curly braces *{...}*. Anything that is not contained in braces is
considered literal text. Literal braces can be escaped by doubling
the brace character *{{* and *}}*, respectively.

A format string has the following form:
::

  replacement_spec ::= "{" [<argument>] ["." <field>] ["[" <index> "]"] [":" format_spec] "}"

The single fields have the following meaning.

**argument**
  A number denoting the argument passed to *fmt*. The first
  argument (after the format string) has number 0. This number can
  be used to refer to a specific argument. The same argument can be
  referred by multiple replacement fields:

  .. code-block:: nim

    "{0} {1} {0}".fmt(1, 0)

  gives the string *"1 0 1"*.

  If no argument number is given, the replacement fields refer to
  the arguments passed to *fmt* in order. Note that this is an
  always-or-never option: either *all* replacement fields use
  explicit argument numbers or none.

**field**
  If the argument is a structured type (e.g. a tuple), this
  specifies which field of the argument should be formatted, e.g.

  .. code-block:: nim

    "{0.x} {0.y}".fmt((x: 1, y:"foo"))

  gives *"1 foo"*.

**index**
  If the argument is a sequence type the index refers to the
  elements of the sequence to be printed:

  .. code-block:: nim

    "<{[1]}>".fmt([23, 42, 81])

  gives *"<42>"*.

**format_spec**
  This is the format specification for the argument as described in
  `Formatting a single value: format`_.

Nested format strings
----------------------
Format strings must be literal strings. Although this might be a
restriction (format strings cannot be constructed during runtime),
nested format strings give back a certain flexibility.

A nested format string is a format string in which the *format
specifier* part of a replacement field contains further replacement
fields, e.g.

  .. code-block:: nim

    "{:{}{}{}x}".fmt(66, ".", "^", 6)

Results in the string *"..42.."*.

*fmt* allows exactly one nested level. Note that the resulting code
is slightly more inefficient than without nesting (but only for
those arguments that actually use nested fields), because after
construction of the outer format specification, the format string
must be parsed again at runtime. Furthermore, the constructed
format string requires an additional temporary string.

The following example demonstrates how *fmt* together with array
separators can be used to format a nested in array in a Matlab-like
style:

  .. code-block:: nim

    "A=[{:6ga|;\n   |, }]".fmt([[1.0,2.0,3.0], [4.0,5.0,6.0]])

results in

  ::

    A=[     1,      2,      3;
            4,      5,      6]

How *fmt* works
---------------
The *fmt* macros transforms the format string and its arguments
into a sequence of commands that build the resulting string. The
format specifications are parsed and transformed into a *Format*
structure at compile time so that no overhead remains at runtime.
For instance, the following expression

  .. code-block:: nim

    "This {} the number {:_^3} example".fmt("is", 1)

is roughly transformed to

  .. code-block:: nim

    (let arg0 = "is";
     let arg1 = 1;
     var ret = newString(0);
     addformat(ret, "This ");
     addformat(ret, arg0, DefaultFmt);
     addformat(ret, " the number ");
     addformat(ret, arg1, Format(...));
     addformat(ret, " example ");
     ret)

(Note that this is a statement-list-expression). The functions
*addformat* are defined within *strfmt* and add formatted output to
the string *ret*.

String interpolation *interp*
-----------------------------

------

**Warning:** This feature is highly experimental.

------

The *interp* macro interpolates a string with embedded
expressions. If the string to be interpolated contains a *$*, then
the following characters are interpreted as expressions.

  .. code-block:: nim

    let x = 2
    let y = 1.0/3.0
    echo interp"Equation: $x + ${y:.2f} == ${x.float + y}"

The macro *interp* supports the following interpolations
expressions:

  ====================== ===========================================
  String                 Meaning
  ---------------------- -------------------------------------------
  ``$<ident>``           The value of the variable denoted by
                         ``<ident>`` is substituted into the string
                         according to the default format for the
                         respective type.
  ``${<expr>}``          The expression ``<expr>`` is evaluated and
                         its result is substituted into the string
                         according to the default format of its
                         type.
  ``${<expr>:<format>}`` The expression ``<expr>`` is evaluated and
                         its result is substituted into the string
                         according to the format string
                         ``<format>``. The format string has the
                         same structure as for the *format*
                         function.
  ``$$``                 A literal ``$``
  ====================== ===========================================


How *interp* works
------------------
The macro *interp* is quite simple. A string with embedded
expressions is simply transformed to an equivalent expression using
the *fmt* macro:

  .. code-block:: nim

    echo interp"Equation: $x + ${y:.2f} == ${x.float + y}"

is transformed to

  .. code-block:: nim

    echo fmt("Equation: {} + {:.2f} == {}", x, y, x.float + y)

Writing formatted output to a file: *writefmt*
----------------------------------------------
The *writefmt* family of macros are convenience helpers to write
formatted output to a file. A call

.. code-block:: nim

  writefmt(f, fmtstr, arg1, arg2, ...)

is equivalent to

.. code-block:: nim

  write(f, fmtstr.fmt(arg1, arg2, ...))

However, the former avoids the creation of temporary intermediate
strings (the variable *ret* in the example above) but writes
directly to the output file. The *printfmt* family of functions
does the same but writes to *stdout*.

Adding new formatting functions
-------------------------------
In order to add a new formatting function for a type *T* one has to
define a new function

.. code-block:: nim

  proc writeformat(o: var Writer; x: T; fmt: Format)

The following example defines a formatting function for
a simple 2D-point data type. The format specification is used for
formatting the two coordinate values.

.. code-block:: nim

  type Point = tuple[x, y: float]

  proc writeformat*(o: var Writer; p: Point; fmt: Format) =
    write(o, '(')
    writeformat(o, p.x, fmt)
    write(o, ',')
    write(o, ' ')
    writeformat(o, p.y, fmt)
    write(o, ')')
