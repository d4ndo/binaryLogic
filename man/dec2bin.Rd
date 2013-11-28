\name{dec2bin}
\alias{dec2bin}
\title{Converts a decimal or hex number to a binary digit.}
\usage{
  dec2bin(num, littleEndian=FALSE, signed=FALSE, size=2)
}
\arguments{
  \item{num}{integer hex or decimal number.}

  \item{littleEndian}{if TRUE. Big Endian if FALSE.}

  \item{signed}{TRUE or FALSE. Unsigned by default. (two's
  complement)}

  \item{size}{Needed if signed is set. (by default 2 Byte)}
}
\value{
  The binary number. Returns a logical vector.
}
\description{
  Converts a decimal or hex number to a binary number.
}
\details{
  The binary number is represented by a logical vector. The
  Bit order usually follows the same endianness as the byte
  order. No floating-point support. <- Time B
}
\examples{
dec2bin(0xfa)
as.integer(dec2bin(0xfa))
dec2bin(3)
dec2bin(-1, signed=TRUE, size=1)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
}

