\name{binPrefix2Byte}
\alias{binPrefix2Byte}
\title{Binary prefix to Byte. (KiB,MiB,..)}
\usage{
  binPrefix2Byte(n, prefix="KiB")
}
\arguments{
  \item{n}{vector of numeric values}

  \item{prefix}{binary prefix * Byte. Expeting a »string«}
}
\value{
  The number of bytes fitting in n * binary prefix * Byte
}
\description{
  Amount of Bytes needed to fit in n * KiB, MiB ..etc.
}
\examples{
binPrefix2Byte(c(0.5,1:10),"KiB")
as.integer(dec2bin(binPrefix2Byte(1,"KiB")*Byte()))
}

