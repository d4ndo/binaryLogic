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
x <- as.binary(c(1,1,0,0)); print(x); summary(x);
y <- switchEndianess(x); print(y); summary(y);
}
\seealso{
\link{negate} or \link{fillBits}.
}

