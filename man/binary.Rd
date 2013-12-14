\name{binary}
\alias{binary}
\title{Binary Vector}
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
Create objects of type "binary" vector.
}
\details{
The binary number is represented by a logical vector. The
Bit order usually follows the same endianess as the byte
order. How to read: \itemize{ \item Little Endian (LSB)
---> (MSB) \item Big Endian (MSB) <--- (LSB) } The »Big
Endian« endianess stores its MSB at the lowest adress and
the »Little Endian« endianess stores its MSB at the highest
adress.

e.g. b <-binary(8). \itemize{ \item »Little Endian« : MSB
is at b[1] and LSB is at b[8]. \item »Big Endian« : LSB is
at b[1] and MSB is at b[8]. } Additional information: A
vector in GNU R starts at 1 and not 0 like in C.
Performance: This binary class is just not that great at
heavy number crunching, but it brings some benefits.
Especially if you like to work using vectors in R. It is no
problem to switch from logical to binary and vice versa. No
floating-point support.
}
\examples{
b <- binary(8)
}
\seealso{
\link{as.binary} and \link{is.binary}. To convert a binary
to raw please use \link{as.raw} (pay attention to the
endianness).
}

