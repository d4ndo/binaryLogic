\name{shiftLeft}
\alias{shiftLeft}
\title{Binary Left Shift (<<)}
\usage{
  shiftLeft(x, size)
}
\arguments{
  \item{x}{The binary number to shift. (logical vector).}

  \item{size}{The number of places to shift.}
}
\value{
  Pushes 0's(FALSE) to the vector from right(LSB) to
  left(MSB). Everything on right(MSB) side drops out.
  Returns a logical vector
}
\description{
  Logical left shift x << size
}
\examples{
x <- c(TRUE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE); x
shiftLeft(x,1)
shiftLeft(x,2)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

