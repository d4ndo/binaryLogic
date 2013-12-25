\name{bytesNeeded}
\alias{bytesNeeded}
\title{Minimum number of »Byte« needed to hold n »Bit«}
\usage{
bytesNeeded(n)
}
\arguments{
  \item{n}{The number of Bit.}
}
\value{
The number of minimum Byte needed to hold n Bit.
}
\description{
A simple helper function that returns the minimum number of
Byte needed to hold the amount of n Bit.
}
\examples{
ten <- as.binary(10)
bytesNeeded(length(ten))
}
\seealso{
\link{addUpToByte} or \link{binPrefix2Byte} or \link{Byte}
}

