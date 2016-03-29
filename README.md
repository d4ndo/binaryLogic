<!-- README.md is generated from README.Rmd. Please edit that file -->
binaryLogic
===========

[![Build Status](https://travis-ci.org/d4ndo/binaryLogic.png)](https://travis-ci.org/d4ndo/binaryLogic) [![Downloads](http://cranlogs.r-pkg.org/badges/binaryLogic?color=brightgreen)](http://www.r-pkg.org/pkg/binaryLogic) [![CRAN version](http://www.r-pkg.org/badges/version/binaryLogic)](http://cran.r-project.org/package=binaryLogic)

Binary Logic GNU R Package

Convert, negate, shift and rotate binary digits. (switchEndianess, bytesNeeded, binaryPrefix, fillUpToByte).

Installation
------------

devtools are required to install "binaryLogic" from github: [devtools](https://github.com/hadley/devtools)

``` r
library(devtools)

# install 'binaryLogic'
install_github("d4ndo/binaryLogic")
library(binaryLogic)
```

Getting started
---------------

Starting with a simple conversion. Decimal (Base10) to binary (Base2) and vice versa.

``` r
the_answer_to_the_ultimate_question_of_life_the_universe_and_everything <- as.binary(42)

the_answer_to_the_ultimate_question_of_life_the_universe_and_everything
#> [1] 1 0 1 0 1 0

as.numeric(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)
#> [1] 42

summary(the_answer_to_the_ultimate_question_of_life_the_universe_and_everything)
#>   Signedness  Endianess value<0 Size[bit] Base10
#> 1   unsigned Big-Endian   FALSE         6     42
```

Operator
--------

Behavior »Class Binary«

| Operator              | Behavior                                                        |
|:----------------------|:----------------------------------------------------------------|
| [== or !=]            | Comparision by value.                                           |
| [\<, \<= or \> , \>=] | Comparision by value.                                           |
| [+ or -]              | Operations by value.                                            |
| [\*, ^]               | Operations by value.                                            |
| [/]                   | Not supported.                                                  |
| [&, ¦, xor]           | Bitwise Operations. The smaller vector is filled up with zeros. |
| [!]                   | Indicates logical negation (NOT). Bitwise Operations            |

The logical == operator compares every element of the vector (Bitwise comparison). e.g.

``` r
two <- as.binary(2); two <- as.logical(two); two == two;
#> [1] TRUE TRUE
```

The binary == operator compares the value and it does not distinguish between big and little endian.

``` r
two <- as.binary(2); two == two;
#> [1] TRUE
```

BinaryLogic operators:

| Operator                              | Behavior                                               |
|:--------------------------------------|:-------------------------------------------------------|
| shiftLeft(binary), shiftRight(binary) | shift Operation.                                       |
| rotate(binary)                        | shift Operation.                                       |
| negate(binary)                        | Indicates arithmetic negation. value \<- value \* (-1) |
| switchEndianess(binary)               |                                                        |

Information
-----------

This class is just not that great at heavy number crunching, but it brings some benefits. Especially if you like to work using vectors in R. The »binary« class inherits from the »logical« class. Some function from package `binaryLogic` can be applied to logical vectors such as shift or rotate (see help).

The internal structure looks like this. It is composed of a »logical vector« and several attributes. In this example(Big-Endian), it corresponds to the value = 2(Base10).

``` r
structure(c(TRUE, FALSE), class = c("binary", "logical"), signed = FALSE, littleEndian = FALSE)
#> [1] 1 0
```

The binary number is represented by a logical vector. The Bit order usually follows the same endianess as the byte order. How to read:

-   Little Endian (LSB) —\> (MSB)

-   Big Endian (MSB) \<— (LSB)

The Big Endian endianess stores its MSB at the lowest adress. The Little Endian endianess stores its MSB at the highest adress.

e.g.

``` r
b <-binary(8)
b
#> [1] 0 0 0 0 0 0 0 0
```

-   »Little Endian« : MSB at b[1] and LSB at b[8].

-   »Big Endian« : LSB at b[1] and MSB at b[8].

More Converting
---------------

### Integer

``` r
as.binary(0xAF)
#> [1] 1 0 1 0 1 1 1 1

as.binary(42)
#> [1] 1 0 1 0 1 0

as.binary(42, littleEndian=TRUE)
#> [1] 0 1 0 1 0 1

as.binary(c(0xAF, 0xBF, 0xFF))
#> [[1]]
#> [1] 1 0 1 0 1 1 1 1
#> 
#> [[2]]
#> [1] 1 0 1 1 1 1 1 1
#> 
#> [[3]]
#> [1] 1 1 1 1 1 1 1 1

as.binary(c(2,4,8,16), signed=TRUE, size=1)
#> [[1]]
#> [1] 0 0 0 0 0 0 1 0
#> 
#> [[2]]
#> [1] 0 0 0 0 0 1 0 0
#> 
#> [[3]]
#> [1] 0 0 0 0 1 0 0 0
#> 
#> [[4]]
#> [1] 0 0 0 1 0 0 0 0

as.binary(-1, signed=TRUE, size=1)
#> [1] 1 1 1 1 1 1 1 1
```

other way around

``` r
two <- as.binary(2, signed=TRUE, size=4)
as.integer(negate(two))
#> [1] -2
# or
as.double(two)
#> [1] 2
# alias for
as.numeric(two)
#> [1] 2
```

### Logical

``` r
as.binary(c(1,1,0), signed=TRUE, logic=TRUE)
#> [1] 0 0 0 0 0 1 1 0

as.binary(c(TRUE,TRUE,FALSE), logic=TRUE)
#> [1] 1 1 0

bigEndian <- as.binary(c(1,1,0,0), logic=TRUE)
summary(bigEndian)
#>   Signedness  Endianess value<0 Size[bit] Base10
#> 1   unsigned Big-Endian   FALSE         4     12

littleEndian <- switchEndianess(bigEndian)
print(littleEndian)
#> [1] 0 0 1 1

littleEndian <- as.binary(bigEndian, littleEndian=TRUE)
print(littleEndian)
#> [1] 1 1 0 0

summary(littleEndian)
#>   Signedness     Endianess value<0 Size[bit] Base10
#> 1   unsigned Little-Endian   FALSE         4      3
```

other way around

``` r
as.logical(as.binary(2))
#> [1]  TRUE FALSE
```

### Raw

``` r
b <- as.binary(charToRaw("A"))
summary(b)
#>   Signedness  Endianess value<0 Size[bit] Base10
#> 1   unsigned Big-Endian   FALSE         7     65

as.raw(b)
#> [1] 41
```

### Gray code

``` r
b <- as.binary(0:7, n=3)
g <- lapply(b, bin2gray)
print(g)
#> [[1]]
#> [1] FALSE FALSE FALSE
#> 
#> [[2]]
#> [1] FALSE FALSE  TRUE
#> 
#> [[3]]
#> [1] FALSE  TRUE  TRUE
#> 
#> [[4]]
#> [1] FALSE  TRUE FALSE
#> 
#> [[5]]
#> [1]  TRUE  TRUE FALSE
#> 
#> [[6]]
#> [1] TRUE TRUE TRUE
#> 
#> [[7]]
#> [1]  TRUE FALSE  TRUE
#> 
#> [[8]]
#> [1]  TRUE FALSE FALSE

gray2bin(g[[8]])
#> [1] 1 1 1
```

Special Case
------------

Be aware about this kind of notation »0xAF«. Because Gnu R converts this to an integer first and then it will be converted to a binary digit. This is just a limitation, if you want to use a little endian formation. It can be fixed by using switchEndianess setting the stickyBits=TRUE.

``` r
#Watch out for this
as.binary(0xAF)
#> [1] 1 0 1 0 1 1 1 1
as.binary(0xAF, littleEndian=TRUE)
#> [1] 1 1 1 1 0 1 0 1
#It should behave as follows
as.binary(c(1,0,1,0,1,1,1,1), logic=TRUE)
#> [1] 1 0 1 0 1 1 1 1
as.binary(c(1,0,1,0,1,1,1,1), littleEndian=TRUE, logic=TRUE)
#> [1] 1 0 1 0 1 1 1 1

bigEndian <- as.binary(c(0,0,1,1), logic=TRUE)
bigEndian
#> [1] 0 0 1 1
summary(bigEndian)
#>   Signedness  Endianess value<0 Size[bit] Base10
#> 1   unsigned Big-Endian   FALSE         4      3
littleEndian <- switchEndianess(bigEndian)
littleEndian
#> [1] 1 1 0 0
summary(littleEndian)
#>   Signedness     Endianess value<0 Size[bit] Base10
#> 1   unsigned Little-Endian   FALSE         4      3
littleEndian2 <- switchEndianess(bigEndian, stickyBits = TRUE)
littleEndian2
#> [1] 0 0 1 1
summary(littleEndian2)
#>   Signedness     Endianess value<0 Size[bit] Base10
#> 1   unsigned Little-Endian   FALSE         4     12
```
