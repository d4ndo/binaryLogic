\name{bin2dec}
\alias{bin2dec}
\title{Converts a binary number to a decimal or hex number.}
\usage{
  bin2dec(bin, littleEndian=FALSE, signed=FALSE, hex=FALSE)
}
\arguments{
  \item{bin}{binary number. Any logical vector.}

  \item{littleEndian}{if TRUE. Big Endian if FALSE.}

  \item{signed}{TRUE or FALSE. Unsigned by default. (two
  komplement)}

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
  No floating-point support. Little Endian (LSB) ---> (MSB)
  Big Endian (MSB) <--- (LSB)
}
\examples{
bin2dec(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE))
bin2dec(c(TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE), hex=TRUE)
bin2dec(as.logical(c(1,0,1,1,1,0,1,1)), hex=TRUE)
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer,
  base::raw
}

