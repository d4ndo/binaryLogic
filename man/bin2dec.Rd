\name{bin2dec}
\alias{bin2dec}
\title{Converts a binary(Base2) number to a decimal(Base10) number.}
\usage{
bin2dec(bin)
}
\arguments{
  \item{bin}{binary number. Any logical or binary vector.}
}
\value{
The decimal(Base10) number (numeric/double).
}
\description{
Converts a binary(Base2) number to a decimal(Base10)
number.
}
\details{
No floating-point support. \itemize{ \item Little Endian
(LSB) ---> (MSB) \item Big Endian (MSB) <--- (LSB) }
}
\seealso{
\link{dec2bin} and \link{binary} or \link{logical}
}

