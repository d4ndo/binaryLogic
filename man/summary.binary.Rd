\name{summary.binary}
\alias{summary.binary}
\title{Summary method for binary number.}
\usage{
## S3 methode for class 'binary'
summary(x, ...)
}
\arguments{
  \item{x}{binary vector.}

  \item{...}{further arguments.}
}
\value{
Contains the following information: \itemize{ \item
Signedness : unsigned or signed \item Endianess :
Big-Endian or Little-Endian \item value<0 : negative or
positve number \item Size[Bit] : Size in Bit \item Base10 :
Decimal(Base10) number. }
}
\description{
This method provides information about the attributes of
the binary number.
}
\examples{
summary(negate(as.binary(2, signed=TRUE, littleEndian=TRUE, size=1)))
}
\seealso{
\link{print.binary}
}

