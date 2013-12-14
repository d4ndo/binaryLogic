\name{binAdd}
\alias{binAdd}
\title{Binary Addition (+)}
\usage{
binAdd(x, y)
}
\arguments{
  \item{x}{summand 1 (binary vector)}

  \item{y}{summand 2 (binary vector)}
}
\value{
The sum of x and y. Returns a binary vector.
}
\description{
Adds two binary numbers. (x + y)
}
\details{
Little-Endian is not supported at the moment. No floating
point supported. if x or y is signed the return value will
also be signed.
}
\examples{
five <- dec2bin(5); ten <- dec2bin(10);
bin2dec(binAdd(ten, five))
binAdd(as.binary(c(0,1)),as.binary(c(1,0)))
}
\seealso{
base::as.logical , base::is.logical, base::raw
}

