\name{addUpToByte}
\alias{addUpToByte}
\title{Add up to Byte (000..)}
\usage{
addUpToByte(x, value=FALSE, size=0)
}
\arguments{
  \item{x}{The binary number to add up with Bit's. (Any
  binary vector).}

  \item{value}{add up with FALSE's or add up with TRUE's.}

  \item{size}{in Byte. 0 = auto (smallest possible Byte).}
}
\value{
binary number. A binary vector with the desired size.
}
\description{
Adds up the binary number with Bit to the size in Byte.
}
\details{
No floating point supported.
}
\examples{
addUpToByte(as.binary(c(1,1)), size=2)
addUpToByte(as.binary(c(1,0,1)), value=FALSE, size=2)
}
\seealso{
\link{bytesNeeded} or \link{negate} or
\link{switchEndianess}.
}

