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
  a binary vector of length n
}
\description{
  Create objects of type "binary" vector.
}
\details{
  The binary number is represented by a logical vector. The
  Bit order usually follows the same endianness as the byte
  order. No floating-point support. Little Endian (LSB)
  ---> (MSB) Big Endian (MSB) <--- (LSB)
}
\seealso{
  base::as.logical , base::is.logical, base::raw
}

