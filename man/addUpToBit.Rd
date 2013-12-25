\name{addUpToBit}
\alias{addUpToBit}
\title{Add up to Bit (000..)}
\usage{
addUpToBit(x, n, value=FALSE)
}
\arguments{
  \item{x}{The binary number to add up with Bit's. (Any
  binary vector).}

  \item{value}{add up with FALSE's or add up with TRUE's.}

  \item{n}{size in Bit.}
}
\value{
binary number. A binary vector with the desired size.
}
\description{
Adds up the binary number with Bit to the size n in Bit.
}
\details{
No floating point supported.
}
\examples{
addUpToBit(as.binary(c(1,1)), n=4)
addUpToBit(as.binary(c(1,0,1)), n=4, value=FALSE)
}
\seealso{
\link{addUpToByte}.
}

