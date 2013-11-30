\name{rotate}
\alias{rotate}
\title{Rotate no carry ()}
\usage{
  rotate(x, size)
}
\arguments{
  \item{x}{The binary number to rotate. (binary or logical
  vector).}

  \item{size}{The number of places to rotate.}
}
\value{
  rotates the vector from left to right. The value from MSB
  is used to fill up the vector at LSB. Returns a
  binary/logical vector.
}
\description{
  A circular shift
}
\examples{
x <- as.binary(c(1,0,0,1,1,1,0,1)); x
rotate(x,1)
rotate(x,2)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

