\name{switchEndianess}
\alias{switchEndianess}
\title{Switch Endianess.}
\usage{
  switchEndianess(x)
}
\arguments{
  \item{x}{binary number. Any binary vector.
  switchEndianess(y)}
}
\value{
  switch little-endian to big-endian and vice versa.
}
\description{
  switch little-endian to big-endian and vice versa.
}
\examples{
x <- as.binary(c(1,1,0,0), littleEndian=TRUE); print(x); attributes(x);
y <- switchEndianess(x); print(y); attributes(y);
}
\seealso{
  binaryLogic::as.binary, binaryLogic::is.binary
}

