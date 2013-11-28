\name{binAdd}
\alias{binAdd}
\title{Binary Addition (+)}
\usage{
  binAdd(x, y, signed=FALSE, size=0)
}
\arguments{
  \item{x}{summand 1 (logical vector)}

  \item{y}{summand 2 (logical vector)}

  \item{signed}{TRUE or FALSE. Returns a signed number. see
  example}

  \item{size}{in Byte. Is needed if signed is set. 0 =
  auto}
}
\value{
  The sum of x and y. Returns a logical vector.
}
\description{
  Adds two binary numbers. (x + y)
}
\details{
  Little-Endian is not supported at the moment. No floating
  point supported.
}
\examples{
binAdd(as.logical(c(0,1)),as.logical(c(1,0)))
}
\seealso{
  base::as.logical , base::is.logical, base::as.integer
  base::raw
}

