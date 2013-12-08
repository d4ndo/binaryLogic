\name{dec2bin}
\alias{dec2bin}
\title{Converts a decimal or hex number to a binary digit.}
\usage{
  dec2bin(num, signed=FALSE, littleEndian=FALSE, size=2)
}
\arguments{
  \item{num}{integer (hex) or (decimal) number.}

  \item{signed}{TRUE or FALSE. Unsigned by default. (two's
  complement)}

  \item{littleEndian}{if TRUE. Big Endian if FALSE.}

  \item{size}{Needed if signed is set. (by default 2 Byte)}
}
\value{
  The binary number. Returns a binary and logical vector.
}
\description{
  Converts a decimal or hex number to a binary number.
}
\details{
  The binary number is represented by a logical vector. The
  Bit order usually follows the same endianness as the byte
  order. No floating-point support. \itemize{ \item Little
  Endian (LSB) ---> (MSB) \item Big Endian (MSB) <--- (LSB)
  }
}
\examples{
dec2bin(0xfa)
dec2bin(3)
dec2bin(-1, signed=TRUE, size=1)
}
\seealso{
  \link{bin2dec} and \link{binary} or \link{logical}
}

