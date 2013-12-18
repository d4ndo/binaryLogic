binaryLogic
===========

[![Build Status](https://travis-ci.org/d4ndo/binaryLogic.png)](https://travis-ci.org/d4ndo/binaryLogic)

Binary Logic GNU R Package

Convert, negate, shift and rotate binary digits.
(fillBits, switchEndianess, bytesNeeded, binaryPrefix).

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

Starting with a simple conversion. »decimal to binary« and vice versa.
```R
the_answer_to_the_ultimate_question_of_life_the_universe_and_everything <- dec2bin(42)

[1] 1 0 1 0 1 0

bin2dec(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)

[1] 42

summary(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)
```

Information
-----------

The »binaryLogic::binary« class inherits from the »base::logical« class. This class is just not that great at heavy number crunching, but it brings some benefits. Especially if you like to work using vectors in R. It is no problem to switch from logical to binary and vice versa. Some function from Package binaryLogic can also be used on logical vectors like shiftLeft, shiftRight, rotate (see help).

Some operators have a different behavior. The logical == operator compares every element of the vector (Bitwise comparison). e.g. 

```R
$ two <- dec2bin(2); two <- as.logical(two); two == two;

[1] TRUE TRUE
```
The binary == operator compares the value.

```R
$ two <- as.binary(two); two == two;

[1] TRUE
```
 Even different endianess.
 
```R
$ two_B <- as.binary(two,littleEndian=FALSE); two_L <- as.binary(two,littleEndian=TRUE);  two_B == two_L;

[1] TRUE
```
More Converting
---------------

Convert from ``raw`` to binary and vice versa.
```R
as.binary(rawToBits(raw)); as.raw(binary);
```

Convert from ``logical`` to binary and vice versa.
```R
as.binary(logical); as.logical(binary);
```

Convert from ``numeric`` to binary. ``numeric`` as type of  ``integer`` or ``double``. No floating point support until v1.0.
```R
dec2bin(numeric); 
# or
as.binary(c(1,0,1,0,1,0));
```

Convert from binary to ``numeric`` as type of ``double`` or ``integer``.
```R
as.integer(binary)
# or
as.double(binary)
# alias for
bin2dec(binary);
```
.



