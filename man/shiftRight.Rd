\name{shiftRight}
\alias{shiftRight}
\title{Binary Right Shift (>>)}
\usage{
  shiftRight(x, size)
}
\arguments{
  \item{x}{The binary number to shift. (binary or logical
  vector).}

  \item{size}{The number of places to shift.}
}
\value{
  Pushes 0's(FALSE) to the vector from left(MSB) to
  right(LSB). Everything on right(LSB) side drops out.
  Returns a binary/logical vector
}
\description{
  Logical right shift 1 >> size
}
\examples{
x <- as.binary(c(1,0,0,1,1,1,0,1)); x
shiftRight(x,1)
shiftRight(x,2)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

