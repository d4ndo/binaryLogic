\name{binarySeq}
\alias{binarySeq}
\title{binarySeq}
\usage{
binarySeq(s=0:15, ...)
}
\arguments{
  \item{s}{a sequence. 1:8 by default}

  \item{...}{used for dec2bin()}
}
\value{
a sequence list of binary digits.
}
\description{
binary Sequence
}
\examples{
l <- binarySeq(0:15, size=1); l;
l <- binarySeq(0:15, signed=TRUE, littleEndian=TRUE, size=1); l;
}
\seealso{
\link{binary}
}

