\name{ta}
\alias{ta}
\title{Temporal Aggregation of Time Series (deprecated)}
\usage{
ta(x, ...)
}
\arguments{
  \item{x}{a time series object of class \code{"ts"} or
  \code{"mts"}.}

  \item{conversion}{type of conversion: \code{"sum"},
  \code{"average"}, \code{"first"} or \code{"last"}.}

  \item{to}{(low-frequency) destination frequency as a
  character string (\code{"annual"} or \code{"quarterly"})
  or as a scalar (e.g. \code{1}, \code{2}, \code{4}).}

  \item{...}{additional arguments, passed to the methods.}
}
\value{
\code{ta} returns an object of class \code{"ts"} or
\code{"mts"}, depending on the class of the input series.
}
\description{
Function is deprecated and will be defunct soon. Use the
more general convert function instead. See examples in
?convert.
}
\examples{
data(swisspharma)

sales.q.a <- ta(sales.q, conversion = "sum", to = "annual")
all.equal(sales.a, sales.q.a)
}
\seealso{
\code{\link{td}} for the main function for temporal
disaggregation.
}
\keyword{models}
\keyword{ts,}

