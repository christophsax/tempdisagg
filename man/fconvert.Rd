\name{fconvert}
\alias{fconvert}
\title{General Frequency Conversion}
\usage{
fconvert(x, to = "quarterly", conversion = "sum",
  method = "denton-cholette", ...)
}
\arguments{
  \item{x}{an object to temporally convert, usually an
  object of class \code{"ts"} or \code{"mts"}.}

  \item{to}{destination frequency as a character string
  ("annual", "quarterly" or "monthly") or as a scalar (e.g.
  2, 4, 7, 12).}

  \item{conversion}{type of temporal conversion:
  \code{"sum"}, \code{"average"}, \code{"first"} or
  \code{"last"}.}

  \item{method}{method of temporal disaggregation without
  an indicator: \code{"denton-cholette"}, \code{"denton"},
  \code{"uniform"} or \code{"ols"}. Only has an effect if
  destination frequency is higher. See \code{\link{td}} for
  details.}
}
\value{
an object of the same class as \code{x}, with frequency
converted. If the destination frequency is equal to the
orginal frequency, the input is returned.
}
\description{
Converts the frequency of a time series object to any other
frequency. Performs temporal disaggregation if the target
frequency is higher and temporal aggregation if the target
frequency is lower.
}
\details{
Preserves NA values at the beginning and at the end.
Internal NAs result in a series that only contains NAs (see
examples).
}
\examples{
fconvert(cbind(mdeaths, fdeaths), to = 1)  # handling "mts" objects
fconvert(austres, to = 12)                 # disaggregation
fconvert(austres, to = 1)                  # aggregation

# NAs are perserved, incomplete periods are omitted
fconvert(window(mdeaths, end = c(1982, 5), extend = TRUE), to = 1)

airmiles.q <- fconvert(airmiles, to = 4)
all.equal(fconvert(airmiles.q, to = 1), airmiles)
}

