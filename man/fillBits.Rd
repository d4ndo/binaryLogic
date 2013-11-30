\name{fillBits}
\alias{fillBits}
\title{Fill Bits (000..)}
\usage{
  fillBits(x, value=FALSE, littleEndian=FALSE, size=0)
}
\arguments{
  \item{x}{The binary number to fill with bits. (binary or
  logical vector).}

  \item{value.}{fill with FALSE or fill with TRUE.}

  \item{littleEndian}{if TRUE. Big Endian if FALSE.}

  \item{size}{in Byte. 0 = auto (smallest possible byte).}
}
\value{
  binary number. A binary / logical vector with the desired
  size.
}
\description{
  Fills the number with Bits to the size in Byte.
}
\details{
  No floating point supported.
}
\examples{
fillBits(as.binary(c(0,0)), value=TRUE)
fillBits(as.logical(c(1,1)), size=2)
fillBits(as.binary(c(TRUE,FALSE,TRUE)), littleEndian=TRUE, value=FALSE, size=2)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

