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

Information
-----------

This class is just not that great at heavy number crunching, but it brings some benefits. Especially if you like to work using vectors in R. The »binary« class inherits from the »logical« class. Some function from Package binaryLogic can also be used on logical vectors like shiftLeft, shiftRight, rotate (see help).

Some operators have a different behavior. The logical == operator compares every element of the vector (Bitwise comparison). e.g. 

```R
$ two <- as.binary(2); two <- as.logical(two); two == two;

[1] TRUE TRUE
```
The binary == operator compares the value. It does not distinguish between big and little endian.

```R
$ two <- as.binary(two); two == two;

[1] TRUE
```
 
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



