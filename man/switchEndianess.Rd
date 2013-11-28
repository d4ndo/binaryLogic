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
x <- c(TRUE,TRUE,FALSE,FALSE); x
y <- switchEndianess(x); y
switchEndianess(y)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer,
  base::raw
}

