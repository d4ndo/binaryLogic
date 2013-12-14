\name{fillBits}
\alias{fillBits}
\title{Fill Bits (000..)
!c(rep(0,Byte()-length(x)),x)}
\usage{
fillBits(x, value=FALSE, size=0)
}
\arguments{
  \item{x}{The binary number to fill with bits. (Any binary
  vector).}

  \item{value}{Fill with FALSE or fill with TRUE.}

  \item{size}{in Byte. 0 = auto (smallest possible byte).}
}
\value{
binary number. A binary vector with the desired size.
}
\description{
Fills the number with Bits to the size in Byte.
}
\details{
No floating point supported.
}
\examples{
fillBits(as.binary(c(1,1)), size=2)
fillBits(as.binary(c(1,0,1)), value=FALSE, size=2)
}
\seealso{
\link{bytesNeeded} or \link{binPrefix2Bytes} or \link{Byte}
}

