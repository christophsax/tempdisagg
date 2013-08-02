\name{td}
\alias{td}
\title{Temporal Disaggregation of Time Series}
\usage{
  td(formula, conversion = "sum", to = "quarterly",
    method = "chow-lin-maxlog", truncated.rho = 0,
    fixed.rho = 0.5, criterion = "proportional", h = 1,
    start = NULL, end = NULL, ...)
}
\arguments{
  \item{formula}{an object of class
  \code{"\link{formula}"}: a symbolic description of the
  the temporal disaggregation model. The details of model
  specification are given under 'Details'.}

  \item{conversion}{type of conversion: \code{"sum"},
  \code{"average"}, \code{"first"} or \code{"last"}.}

  \item{method}{method of temporal disaggregation:
  \code{"chow-lin-maxlog"},
  \code{"chow-lin-minrss-ecotrim"},
  \code{"chow-lin-minrss-quilis"}, \code{"chow-lin-fixed"},
  \code{"fernandez"}, \code{"litterman-maxlog"},
  \code{"litterman-minrss"}, \code{"litterman-fixed"},
  \code{"denton-cholette"}, \code{"denton"},
  \code{"uniform"} or \code{"ols"}. See 'Details'.}

  \item{to}{high-frequency destination frequency as a
  character string (\code{"quarterly"} or \code{"monthly"})
  or as a scalar (e.g. \code{2}, \code{4}, \code{7},
  \code{12}). If the input series are \code{ts} objects,
  the argument is necessary if no indicator is given. If
  the input series are vectors, \code{to} must be a scalar
  indicating the frequency ratio.}

  \item{truncated.rho}{lower bound for the autoregressive
  parameter \eqn{\rho}. If set to \code{0} (default), no
  negative values are allowed. If set to \code{-1},
  truncation is disabled.}

  \item{fixed.rho}{set a predefined autoregressive
  parameter \eqn{\rho}. Only works with the methods
  \code{"chow-lin-fixed"} and \code{"litterman-fixed"}.}

  \item{criterion}{minimzation criterion for Denton
  methods: \code{"proportional"} or \code{"additive"}.  See
  'Details'.}

  \item{h}{degree of differencing for Denton methods. See
  'Details'.}

  \item{start}{(optional) start date. Similar to
  pre-processing the input series with
  \code{\link{window}}.}

  \item{end}{(optional) end date. Similar to pre-processing
  the input series with \code{\link{window}}.}

  \item{...}{additional arguments to be passed to the low
  level subfunctions.}
}
\value{
  \code{td} returns an object of class \code{"td"}.

  The function \code{\link[=predict.td]{predict}} computes
  the interpolated high frequency series. If the
  high-frequency indicator series are longer than the
  low-frequency series, the resulting series will be
  extrapolated.  The function \code{coefficients} extracts
  the coefficients. The function \code{residuals} extracts
  the low frequency residuals. The function
  \code{\link[=summary.td]{summary}} prints a summary of
  the estimation.

  An object of class \code{"td"} is a list containing the
  following components: \item{values}{disaggregated or
  interpolated (and extrapolated) high frequency series}
  \item{fitted.values}{low frequency fitted values of the
  regression; low frequency indicator for the Denton
  methods.} \item{p}{preliminary high frequency series}
  \item{residuals}{low-frequency residuals}
  \item{rho}{autoregressive parameter, \eqn{\rho}}
  \item{truncated}{logical, whether \eqn{\rho} has been
  truncated} \item{coefficients}{a named vector of
  coefficients} \item{se}{standard errors of the
  coefficients} \item{s_2}{ML-estimator of the variance of
  the high-frequency residuals}
  \item{s_2_gls}{GLS-estimator of the variance of the
  high-frequency residuals} \item{tss}{weighted (low
  frequency) total sum of squares} \item{rss}{weighted (low
  frequency) residual sum of squares} \item{r.squared}{R
  squared} \item{adj.r.squared}{adjusted R squared}
  \item{logl}{log-likelihood} \item{aic}{Akaike information
  criterion} \item{bic}{Schwarz information criterion}
  \item{rank}{number of right hand variables (including
  intercept)} \item{df}{degrees of freedom}
  \item{method}{method of temporal disaggregation}
  \item{call}{function call} \item{name}{name of the low
  frequency variable} \item{fr}{the ratio of high to
  low-frequency series} \item{conversion}{type of temporal
  conversion} \item{actual}{actual values of the low
  frequeny series} \item{model}{a matrix containing the
  indicators (and a constant if present)}
  \item{criterion}{minimization criterion in Denton
  methods} \item{h}{order of differencing in Denton
  methods}
}
\description{
  Perform temporal disaggregation or interpolation of low
  frequency to high frequency time series. \code{td} can be
  used with objects of class \code{"\link{ts}"} as well as
  with basic vectors.
}
\details{
  \code{td} is used to disaggregate or interpolate a low
  frequency to a higher frequency time series, while either
  the sum, the average, the first or the last value of the
  resulting high-frequency series is consistent with the
  low frequency series. Disaggregation can be performed
  with or without the help of one or more indicator series.
  It can deal with all situations where the high frequency
  is an integer multiple of the low frequency (e.g. weeks
  to days), but not with irregular frequencies (e.g. weeks
  to months).

  The selection of a temporal disaggregation model is
  similar to the selection of a linear regression model.
  Thus, \code{td} closely mirrors the working of the
  \code{\link{lm}} function. The left hand side of the
  \code{\link{formula}} denotes the low-frequency series,
  the right hand side the indicators. If no indicator is
  specified, the right hand side must be set equal to
  \code{1} (see examples). Unlike \code{lm}, \code{td}
  handles \code{\link{ts}} and \code{mts} time-series
  objects, as a typical application involves the use of
  these objects. Alternatively, If used with basic vectors,
  the \code{to} argument specifies the ratio between the
  high and the low frequency series.

  For the generalized least squares (GLS) methods
  \code{"chow-lin-maxlog"},
  \code{"chow-lin-minrss-ecotrim"},
  \code{"chow-lin-minrss-quilis"},
  \code{"litterman-maxlog"} and \code{"litterman-minrss"},
  an autoregressive parameter \eqn{\rho} is estimated.
  Default (and recommended) method is
  \code{chow-lin-maxlog}. With \code{neg.rho = FALSE}
  (default), it produces good results for a wide range of
  applications.

  There are two variants of the \code{chow-lin-minrss}
  approach that lead to different results: Ecotrim by
  Barcellan (2002) uses a correlation matrix instead of the
  variance covariance matrix (implemented in
  \code{"chow-lin-minrss-ecotrim"}), the Matlab library by
  Quilis (2009) multiplies the correlation matrix with
  \eqn{1/(1-\rho^2)} (implemented in
  \code{"chow-lin-minrss-quilis"}).

  The Denton methods \code{"denton"} and
  \code{"denton-cholette"} can be specified with one or
  without an indicator. The parameter \code{h} can be set
  equal to \code{0}, \code{1}, or \code{2}. Depending on
  the value, the \code{denton} procedure minimizes the sum
  of squares of the deviations between the levels
  (\code{0}), the first differences (\code{1}) or the
  second differences (\code{2}) of the indicator and the
  resulting series. Additionally, \code{criterion} can be
  set equal to \code{"proportional"} or \code{"additive"},
  depending on whether the proportional or the absolute
  deviations should be considered for minimzation.
  \code{"denton-cholette"} removes the transient movement
  of the original \code{"denton"} method at the beginning
  of the resulting series.

  \code{"uniform"} is a special case of the \code{"denton"}
  approach, with \code{h} equals \code{0} and
  \code{criterion} equals \code{"proportional"}. It
  distributes the residuals uniformly. If no indicator is
  used, this leads to a step-shaped series.

  \code{"ols"} performs an ordinary least squares
  regression (OLS) and distributes the residuals uniformly.
  It is especially useful for comparing the estimators of
  GLS and OLS regressions.
}
\examples{
data(swisspharma)

# one indicator, no intercept
mod1 <- td(sales.a ~ 0 + exports.q)
summary(mod1)  # summary statistics
plot(mod1)  # residual plot of regression
plot(predict(mod1))

# interpolated quarterly series

# temporally aggregated series is equal to the annual value
all.equal(ta(predict(mod1), conversion = "sum", to = "annual"), sales.a)

# several indicators, including an intercept
mod2 <- td(sales.a ~ imports.q + exports.q)

# no indicator (Denton-Cholette)
mod3 <- td(sales.a ~ 1, to = "quarterly", method = "denton-cholette")

# no indicator (uniform)
mod4 <- td(sales.a ~ 1, to = "quarterly", method = "uniform")

# Example from Denton (1971), see references.
d.q <- ts(rep(c(50, 100, 150, 100), 5), frequency = 4)
d.a <- ts(c(500, 400, 300, 400, 500))

a1 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "additive", h = 0))
a2 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "additive", h = 1))
a3 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "additive", h = 2))
a4 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "additive", h = 3))

p1 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "proportional", h = 0))
p2 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "proportional", h = 1))
p3 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "proportional", h = 2))
p4 <- predict(td(d.a ~ 0 + d.q, method = "denton",
                 criterion = "proportional", h = 3))

# Table in Denton (1971), page 101:
round(cbind(d.q, a1, a2, a3, a4, p1, p2, p3, p4))
}
\references{
  Chow, G. C., & Lin, A. L. (1971). Best linear unbiased
  interpolation, distribution, and extrapolation of time
  series by related series. \emph{The review of Economics
  and Statistics}, 372-375.

  Denton, F. T. (1971). Adjustment of monthly or quarterly
  series to annual totals: an approach based on quadratic
  minimization. \emph{Journal of the American Statistical
  Association}, 66(333), 99-102.

  Barcellan, R. (2002). ECOTRIM: a program for temporal
  disaggregation of time series, Workshop on Quarterly
  National Accounts, Eurostat.

  Quilis, E. M. (2009). Matlab Temporal Disaggregation
  Library. URL:
  \url{http://www.mathworks.com/matlabcentral/fileexchange/24438-temporal-disaggregation-library}
}
\seealso{
  \code{\link{ta}} for temporal aggregation, the inverse
  function of \code{td}.

  \code{\link[=summary.td]{summary}} is used to obtain and
  print a summary of the results.

  \code{\link[=predict.td]{predict}} is used to extract the
  disaggregated or interpolated high frequency series.

  \code{\link[=plot.td]{plot}} is used to plot the
  predicted and actual low frequency series, as well as the
  residuals.
}
\keyword{models}
\keyword{ts,}

