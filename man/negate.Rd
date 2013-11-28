\name{negate}
\alias{negate}
\title{Binary Negation (!)}
\usage{
  negate(x)
}
\arguments{
  \item{x}{The number to be negated. A logical vector is
  expected.}
}
\value{
  The negated number of x. Returns a logical vector with
  signed=TRUE
}
\description{
  Negates the binary number x. Negation x -> -x or -x -> x
}
\details{
  No floating point supported.
}
\examples{
negate(c(TRUE,FALSE,TRUE))
bin2dec(negate(dec2bin(-5, signed=TRUE)))
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

