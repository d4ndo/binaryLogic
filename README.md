binaryLogic
===========

[![Build Status](https://travis-ci.org/d4ndo/binaryLogic.png)](https://travis-ci.org/d4ndo/binaryLogic)

Binary Logic GNU R Package

Convert, negate, shift and rotate binary digits.
(switchEndianess, bytesNeeded, binaryPrefix, fillUpToByte).

## Installation

devtools are required to install "binaryLogic" from github: [devtools](https://github.com/hadley/devtools)

```R
library(devtools)

# install 'binaryLogic'
install_github("d4ndo/binaryLogic")
library(binaryLogic)
```

Getting started
---------------

Starting with a simple conversion. Decimal (Base10) to binary (Base2) and vice versa.

```R
the_answer_to_the_ultimate_question_of_life_the_universe_and_everything <- as.binary(42)

[1] 1 0 1 0 1 0

as.numeric(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)

[1] 42

summary(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)
```

Operator
---------

Operators:


| Operator               | Behavior »Class Binary«                                          | 
|:----------------------:|-----------------------------------------------------------------:|
| == or !=               | Comparision by value.                                            |
| &lt; , &lt;= or > , >= | Comparision by value.                                            |
| + or -                 | Operations by value.                                             |
| *, ^                   | Operations by value.                                             |
| /                      | Not supported.                                                   |
| &, \|, xor             | Bitwise Operations. The smaller vector is filled up  with zeros. |
| !                      | Indicates logical negation (NOT). Bitwise Operations             |

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

| Operator                              | Behavior »Class Binary«                              | 
|:-------------------------------------:|-----------------------------------------------------:|
| shiftLeft(binary), shiftRight(binary) | shift Operation.                                     |
| rotate(binary)                        | shift Operation.                                     |
| negate(binary)                        | Indicates arithmetic negation. value <- value * (-1) |
| switchEndianess(binary)               |                                                      |

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
### Integer

```R
as.binary(0xAF)
[1] 1 0 1 0 1 1 1 1

as.binary(42)
[1] 1 0 1 0 1 0

as.binary(42, littleEndian=TRUE)
[1] 0 1 0 1 0 1

as.binary(c(0xAF, 0xBF, 0xFF))
[[1]]
[1] 1 0 1 0 1 1 1 1

[[2]]
[1] 1 0 1 1 1 1 1 1

[[3]]
[1] 1 1 1 1 1 1 1 1

as.binary(c(2,4,8,16), signed=TRUE, size=1)
[[1]]
[1] 0 0 0 0 0 0 1 0

[[2]]
[1] 0 0 0 0 0 1 0 0

[[3]]
[1] 0 0 0 0 1 0 0 0

[[4]]
[1] 0 0 0 1 0 0 0 0

as.binary(-1, signed=TRUE, size=1)
[1] 1 1 1 1 1 1 1 1
```

other way around

```R
two <- as.binary(2, signed=TRUE, size=4)
as.integer(negate(two))
# or
as.double(two)
# alias for
as.numeric(two)
```

### Logical


```R
as.binary(c(1,1,0), signed=TRUE, logic=TRUE)
[1] 0 0 0 0 0 1 1 0

as.binary(c(TRUE,TRUE,FALSE), logic=TRUE)
[1] 1 1 0

bigEndian <- as.binary(c(1,1,0,0), logic=TRUE)
summary(bigEndian)
Signedness  Endianess value<0 Size[Bit] Base10
unsigned    Big-Endian  FALSE         4     12

littleEndian <- switchEndianess(bigEndian)
print(littleEndian)
[1] 0 0 1 1

littleEndian <- as.binary(bigEndian, littleEndian=TRUE)
print(littleEndian)
[1] 1 1 0 0

summary(littleEndian)
Signedness     Endianess value<0 Size[Bit] Base10
unsigned   Little-Endian   FALSE         4      3
```

other way around

```R
as.logical(as.binary(2))
[1]  TRUE FALSE
```

### Raw

```R
b <- as.binary(charToRaw("A")); 
summary(b);
 Signedness  Endianess value<0 Size[Bit] Base10
 unsigned   Big-Endian   FALSE         7     65

as.raw(b);
[1] 41
```




