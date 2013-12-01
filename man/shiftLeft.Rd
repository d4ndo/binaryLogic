\name{shiftLeft}
\alias{shiftLeft}
\title{Binary Left Shift (<<)}
\usage{
  shiftLeft(x, n)
}
\arguments{
  \item{x}{The binary number to shift. (binary or logical
  vector).}

  \item{n}{The number of bits to shift.}
}
\value{
  Pushes 0's(FALSE) to the vector from right(LSB) to
  left(MSB). Everything on right(MSB) side drops out.
  Returns a binary/logical vector
}
\description{
  Logical left shift x << n
}
\examples{
x <- as.binary(c(1,0,0,1,1,1,0,1)); x
shiftLeft(x,1)
shiftLeft(x,2)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

