binaryLogic
===========

[![Build Status](https://travis-ci.org/d4ndo/binaryLogic.png)](https://travis-ci.org/d4ndo/binaryLogic)

Binary Logic GNU R Package

Convert, negate, shift and rotate binary digits.
(switchEndianess, bytesNeeded, binaryPrefix, addUpToByte).

## Installation

Development version on [github](https://github.com/hadley/devtools)
```R
# development version
library(devtools)

# install 'binaryLogic'
install_github("d4ndo/binaryLogic")
library(binaryLogic)
```

Getting started
---------------

Starting with a simple conversion. Decimal(Base10) to binary(Base2) and vice versa.
```R
the_answer_to_the_ultimate_question_of_life_the_universe_and_everything <- as.binary(42)

[1] 1 0 1 0 1 0

as.numeric(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)

[1] 42

summary(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)
```

Operators
---------

Operators:

<table>
  <tr>
    <th>Operator</th><th>Behavior »Class Binary«</th>
  </tr>
  <tr>
    <td>== or !=</td><td>Comparision by value.</td>
  </tr>
  <tr>
    <td>&lt; , &lt;= or > , >=</td><td>Comparision by value.</td>
  </tr>
  <tr>
    <td>+ or -</td><td>Operations by value.</td>
  </tr>
  </tr>
  <tr>
    <td>*, ^, %%, %/%</td><td>Operations by value.</td>
  </tr>
  <tr>
    <td>/<td>Not supported.</td>
  </tr>  
  <tr>
    <td>&, |, xor</td><td>Bitwise Operations. The smaller vector is added up  with zeros.</td>
  </tr>
  <tr>
    <td>!</td><td>Indicates logical negation (NOT). Bitwise Operations</td>
  </tr>  
</table>

The logical == operator compares every element of the vector (Bitwise comparison). e.g. 

```R
$ two <- as.binary(2); two <- as.logical(two); two == two;

[1] TRUE TRUE
```
The binary == operator compares the value and it does not distinguish between big and little endian.

```R
$ two <- as.binary(two); two == two;

[1] TRUE
```

BinaryLogic operators:

<table>
  <tr>
    <th>Operator</th><th>Behavior »Class Binary«</th>
  </tr>
  <tr>
    <td>shiftLeft(binary), shiftRight(binary)</td><td>shift Operation.</td>
  </tr>
  <tr>
    <td>rotate(binary)</td><td>shift Operation.</td>
  </tr>
  <tr>
    <td>negate(binary)</td><td>Indicates arithmetic negation. value <- value * (-1)</td>
  </tr>
  <tr>
    <td>switchEndianess(binary)</td><td></td>
  </tr>    
</table>


Information
-----------

This class is just not that great at heavy number crunching, but it brings some benefits. Especially if you like to work using vectors in R. The »binary« class inherits from the »logical« class. Some function from package ``binaryLogic`` can be applied to logical vectors such as shift or rotate (see help).

The internal structure looks like this. It is composed of a »logical vector« and several attributes. In this example(Big-Endian), it corresponds to the value = 2(Base10).

```R
structure(c(TRUE, FALSE), class = c("binary", "logical"), signed = FALSE, littleEndian = FALSE)
```

The binary number is represented by a logical vector. The Bit order usually follows the same endianess as the byte order. How to read:

* Little Endian (LSB) —> (MSB)

* Big Endian (MSB) <— (LSB)

The Big Endian endianess stores its MSB at the lowest adress. 
The Little Endian endianess stores its MSB at the highest adress.

e.g. b <-binary(8):

* »Little Endian« : MSB at b[1] and LSB at b[8].

* »Big Endian« : LSB at b[1] and MSB at b[8].


More Converting
---------------

Convert from ``raw`` to binary and vice versa.  ``as.binary(raw) `` - ``as.raw(binary)``. e.g.
```R
r <- packBits(c(rep(T,31), F)); b <- as.binary(r); as.raw(switchEndianess(b));
```

Convert from ``logical`` to binary and vice versa.  ``as.binary(logical)`` - ``as.logical(binary)``. e.g.
```R
b <- as.binary(c(TRUE,TRUE,FALSE,TRUE)); as.logical(addUpToByte(b));
```

Convert from ``numeric`` to binary. ``as.binary(numeric)``. e.g.
```R
as.binary(42)
# A numeric vector is interpreted differently. Same result as above.
as.binary(c(1,0,1,0,1,0))
```

Convert from binary to ``numeric`` as type of ``double`` or ``integer``. ``as.numeric(binary)`` - ``as.double(binary)`` - ``as.integer(binary)``. e.g.
```R
two <- as.binary(2, signed=TRUE, size=4)
as.integer(negate(two))
# or
as.double(two)
# alias for
as.numeric(two)
```
.



