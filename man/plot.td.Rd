\name{plot.td}
\alias{plot.td}
\title{Residual Plot for Temporal Disaggregation}
\usage{
  \method{plot}{td} (x, ...)
}
\arguments{
  \item{x}{an object of class \code{"td"}, usually, a
  result of a call to \code{\link{td}}.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  \code{plot.td} returns a a two panel plot as its side
  effect, showing the predicted and actual low frequency
  series, and the residuals.
}
\description{
  Plot method for class \code{"td"}. Plot the predicted and
  actual low frequency series, and residuals.
}
\examples{
data(swisspharma)

mod2 <- td(sales.a ~ imports.q + exports.q)
plot(mod2)
}
\seealso{
  \code{\link{td}} for the main function for temporal
  disaggregation.
}

