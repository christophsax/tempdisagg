\name{predict.td}
\alias{predict.td}
\title{Predict method for Temporal Disaggregation}
\description{computes the disaggregated or interpolated (and extrapolated) high frequency series of a temporal disaggregation.}
\usage{
\method{predict}{td}(object, \dots)
}
\arguments{
  \item{object}{an object of class \code{"td"}, usually, a result of a call to \code{\link{td}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  \code{summary.td} returns a vector or a \code{"ts"} object, containing the disaggregated or interpolated high frequency series of a temporal disaggregation.
}
\author{
Christoph Sax, Peter Steiner
}


\seealso{
\code{\link{td}} for the main function for temporal disaggregation.}
\examples{
data(swisspharma)

mod1 <- td(sales.a ~ imports.q + exports.q)
predict(mod1)  

}
\keyword{ts, models}
