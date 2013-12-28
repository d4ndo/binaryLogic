\name{binary}
\alias{binary}
\title{Binary Object.}
\usage{
binary(n, signed=FALSE, littleEndian=FALSE)
}
\arguments{
  \item{n}{length of vector. Number of bits}

  \item{signed}{TRUE or FALSE. Unsigned by default. (two's
  complement)}

  \item{littleEndian}{if TRUE. Big Endian if FALSE.}
}
\value{
a binary vector of length n. By default filled with
zeros(0).
}
\description{
Create objects of type binary.
}
\details{
The binary number is represented by a logical vector. The
Bit order usually follows the same endianess as the byte
order. How to read: \itemize{ \item Little Endian (LSB)
---> (MSB) \item Big Endian (MSB) <--- (LSB) } The Big
Endian endianess stores its MSB at the lowest adress. The
Little Endian endianess stores its MSB at the highest
adress.

e.g. b <-binary(8). \itemize{ \item »Little Endian« : MSB
at b[1] and LSB at b[8]. \item »Big Endian« : LSB at b[1]
and MSB at b[8]. } No floating-point support.
}
\examples{
b <- binary(8)
summary(b)
b <- binary(16, signed=TRUE)
summary(b)
b <- binary(32, littleEndian=TRUE)
summary(b)
}
\seealso{
\link{as.binary} and \link{is.binary}. To convert a binary
to raw please use \link{as.raw} (pay attention to the
endianness).
}

