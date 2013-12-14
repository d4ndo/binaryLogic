\name{negate}
\alias{negate}
\title{Binary Negation (!)}
\usage{
negate(x)
}
\arguments{
  \item{x}{The number to be negated. A binary or logical
  vector is expected.}
}
\value{
The negated number of x. Returns a binary/logical vector
with signed=TRUE
}
\description{
Negates the binary number x. Negation x -> -x or -x -> x
}
\details{
No floating point supported.
}
\examples{
negate(dec2bin(2, signed=TRUE))
bin2dec(negate(dec2bin(-5, signed=TRUE)))
}
\seealso{
base::as.logical , base::is.logical, base::raw
}

