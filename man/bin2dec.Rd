\name{bin2dec}
\alias{bin2dec}
\title{Converts a binary number to a decimal or hex number.}
\usage{
  bin2dec(bin, hex=FALSE)
}
\arguments{
  \item{bin}{binary number. Any logical or binary vector.}

  \item{hex}{TRUE or FALSE: if TRUE it returns an hex
  value. by default = FALSE}
}
\value{
  The decimal or hex number »integer«.
}
\description{
  Converts a binary number to a decimal or hex number.
}
\details{
  No floating-point support. \itemize{ \item Little Endian
  (LSB) ---> (MSB) \item Big Endian (MSB) <--- (LSB) }
}
\examples{
bin2dec(as.binary(c(1,0,1,1,1,0,1,1)))
bin2dec(as.binary(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)))
bin2dec(as.binary(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)), hex=TRUE)
}
\seealso{
  \link{dec2bin} and \link{binary} or \link{logical}
}

