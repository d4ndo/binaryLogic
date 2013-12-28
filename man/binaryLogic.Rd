\docType{package}
\name{binaryLogic}
\alias{binaryLogic}
\alias{binaryLogic-package}
\title{Binary Logic GNU R Package}
\description{
This package includes the S3 class \strong{binary}. It can
be used to convert, negate, shift and rotate the binary
digits. (switchEndianess, bytesNeeded, binaryPrefix,
addUpToByte).

Binary operators: \itemize{ \item \strong{==} , \strong{!=}
, \strong{<} , \strong{<=} , \strong{>} , \strong{>=} \item
\strong{+} , \strong{-} , \strong{^}, \strong{*} \item
\strong{&} , \strong{|} , \strong{xor} (Logical Operator.
Bitwise operation. The smaller vector is added up with
zeros) \item \strong{!} (Indicates logical negation (NOT).
Bitwise Operations) }

binaryLogic functions: \itemize{ \item \link{shiftLeft} ,
\link{shiftRight} \item \link{rotate} \item \link{negate}
\item \link{switchEndianess} }

Additional function: (\link{addUpToByte},
\link{addUpToBit}, \link{bytesNeeded},
\link{binPrefix2Byte}, \link{Byte})
}
\details{
This \strong{\link{binary}} class is just not that great at
heavy number crunching, but it brings some benefits.
Especially if you like to work using vectors in R. It
inherits from the \emph{logical} class. Some function from
package \pkg{binaryLogic} can be applied to \emph{logical}
vectors. Such as shift or rotate (see help).

The internal structure looks like this:

\code{structure(c(TRUE, FALSE), class = c("binary",
"logical"), signed = FALSE, littleEndian = FALSE)}

It is composed of a \emph{logical} vector and several
attributes. In this example(Big-Endian), it corresponds to
the value = 2(Base10).

The binary number is represented by a logical vector. The
Bit order usually follows the same endianess as the byte
order. How to read: \itemize{ \item \strong{Little Endian}
(LSB) ---> (MSB) \item \strong{Big Endian} (MSB) <--- (LSB)
} The Big Endian endianess stores its MSB at the lowest
adress. The Little Endian endianess stores its MSB at the
highest adress.

e.g. \code{b <-binary(8)} \itemize{ \item \strong{Little
Endian} : MSB at \code{b[1]} and LSB at \code{b[8]}. \item
\strong{Big Endian} : LSB at \code{b[1]} and MSB at
\code{b[8]}. } No floating-point support.
}

