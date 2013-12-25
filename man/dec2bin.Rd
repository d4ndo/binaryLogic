\name{dec2bin}
\alias{dec2bin}
\title{Converts a decimal(Base10) or hex(Base16) number to a binary(Base2) digit.}
\usage{
dec2bin(num, signed=FALSE, littleEndian=FALSE, size=2)
}
\arguments{
  \item{num}{integer (hex) or (decimal) number.}

  \item{signed}{TRUE or FALSE. Unsigned by default. (two's
  complement)}

  \item{littleEndian}{if TRUE. Big Endian if FALSE.}

  \item{size}{in Byte. Needed if »signed« is set. (by
  default 2 Byte)}
}
\value{
The binary number. Returns a (binary/logical) vector.
}
\description{
Converts a decimal(Base10) or hex(Base16) number to a
binary(Base2) number.
}
\details{
The binary number is represented by a logical vector. The
Bit order usually follows the same endianness as the byte
order. No floating-point support. \itemize{ \item Little
Endian (LSB) ---> (MSB) \item Big Endian (MSB) <--- (LSB) }
Auto switch to signed if num < 0.
}
\seealso{
\link{bin2dec} and \link{binary} or \link{logical}
}

