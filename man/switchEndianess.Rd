\name{switchEndianess}
\alias{switchEndianess}
\title{Switch Endianess.}
\usage{
  switchEndianess(x)
}
\arguments{
  \item{x}{binary number. Any logical vector.}
}
\value{
  switch little-endian to big-endian and vice versa.
}
\description{
  switch little-endian to big-endian and vice versa.
}
\examples{
x <- as.binary(c(1,1,0,0)); x
y <- switchEndianess(x); y
switchEndianess(y)
}
\seealso{
  binaryLogic::as.binary, binaryLogic::is.binary,
  base::as.logical, base::is.logical, base::raw
}

