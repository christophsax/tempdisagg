#' @import stats
#' @method print td
#' @export
print.td <- function(x, ...){
  # calls print.lm
  #
  # Args:
  #   x:           an object of class "td"
  #   ...:         further arguments, not used
  #
  # Returns: 
  #   prints the object with print.lm as a side effect
  
  class(x) <- "lm"
  print(x, ...)

  cat("Use summary() for details. \nUse predict() to extract the final series.
      \nUse ?td to see the help file.\n")
}



#' Summary of a Temporal Disaggregation
#' 
#' \code{summary} method for class "td".
#' 
#' @param object      an object of class \code{"td"}, usually, a result of a 
#'                    call to \code{\link{td}}.
#' @param x           an object of class \code{"summary.td"}, usually, a result 
#'                    of a call to \code{summary.td}.
#' @param digits      the number of significant digits to use when printing.
#' @param signif.stars logical. If \code{TRUE}, 'significance stars' are printed 
#'                    for each coefficient.
#' @param \dots       further arguments passed to or from other methods.
#' @return \code{summary.td} returns a list containing the summary statistics 
#'   included in \code{object}, and computes the following additional
#'   statistics:
#'   
#'   \item{n_l}{number of low frequency observations}
#'   \item{n}{number of high frequency observations}
#'   \item{ar_l}{empirical auto-correlation of the low frequency series}
#'   \item{coefficients}{a named matrix containing coefficients, standard
#'   deviations, t-values and p-values}
#'   
#'   The \code{print} method prints the summary output in a similar way as the method for \code{"lm"}.
#'   
#' @seealso \code{\link{td}} for the main function for temporal disaggregation.
#' @examples
#' data(swisspharma)
#'   
#' mod1 <- td(sales.a ~ imports.q + exports.q)
#' summary(mod1)  
#'   
#' mod2 <- td(sales.a ~ 0, to = "quarterly", method = "uniform")
#' summary(mod2)
#'   
#' @keywords ts, models
#' @method summary td
#' @export
#' 
summary.td <- function(object, ...){
  # build output on top of the input
  z <- object

  # number of observations and number of indicator series
  z$n_l <- length(object$actual)
  if (is.null(dim(object$model))){
    z$n <- length(object$model)
  } else {
    z$n <- dim(object$model)[1]
  }

  # derivative statististics
  z$ar_l           <- cor(z$residuals[-1], z$residuals[-length(z$residuals)])

  # coefficents matrix
  if (!is.null(coef(object))){
    est  <- coef(object)
    se   <- object$se
    tval <- est/se
    pval <- 2 * pnorm(-abs(tval))

    z$coefficients <- cbind(est, se, tval, 
                          2 * pt(abs(tval), z$df, lower.tail = FALSE))
    dimnames(z$coefficients) <- list(names(est),
                                    c("Estimate", "Std. Error", "t value", 
                                    "Pr(>|t|)"))
  }
  class(z) <- "summary.td"
  z
}

#' @method print summary.td
#' @export
#' @rdname summary.td
print.summary.td <- function (x, digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"), ...) {
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n",
      sep = "")
  resid <- x$residuals
  cat("Residuals:\n")
  if (length(resid) > 5) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    quantile.resid <- zapsmall(quantile(resid), digits + 1)
    print(structure(quantile.resid, names = nam), digits = digits)
  } else {
    print(resid, digits = digits)
  }
  if (is.null(coef(x))) {
    cat("\nNo Coefficients\n")
  } else {
      cat("\nCoefficients:\n")
      coefs <- coef(x)
      if (!is.null(aliased <- x$aliased) && any(aliased)) {
        cn <- names(aliased)
        coefs <- matrix(NA, length(aliased), 4, 
                    dimnames = list(cn, colnames(coefs)))
        coefs[!aliased, ] <- x$coefficients
      }
      printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
        na.print = "NA")
  }

  cat("\n'", x$method, "' disaggregation with '", x$conversion, 
      "' conversion", sep = "")
  cat("\n", x$n_l, " low-freq. obs. converted to ", x$n, " high-freq. obs.", sep="")
  if (!is.null(x$adj.r.squared)) {
    cat("\nAdjusted R-squared:", formatC(x$adj.r.squared, digits = digits))
  }
  if (!is.null(x$rho)) {
    cat("\tAR1-Parameter:", formatC(x$rho, digits = digits))
    if (x$truncated){
      cat(" (truncated)")
    }
  }
  if (!is.null(x$criterion)) {
    cat("\ncriterion:", x$criterion, "\torder of differencing 'h':", x$h)
  }
  cat("\n")
  invisible(x)
}


#' Residual Plot for Temporal Disaggregation
#' 
#' \code{plot} method for class \code{"td"}. Plot the fitted and actual low 
#' frequency series, and residuals.
#' 
#' @param x           an object of class \code{"td"}, usually, a result of a 
#'                    call to \code{\link{td}}.
#' @param \dots       further arguments passed to or from other methods.
#' 
#' @return returns a a two panel plot as its side effect, showing
#'   the fitted and actual low frequency series, and the residuals.
#'   
#' @seealso \code{\link{td}} for the main function for temporal disaggregation.
#' 
#' @examples
#' data(swisspharma)
#' 
#' mod2 <- td(sales.a ~ imports.q + exports.q)
#' plot(mod2)  
#' 
#' @keywords ts, models
#' @method plot td
#' @export
#' 
plot.td <- function(x, ...){
  old.par <- par(no.readonly=TRUE)  # backup par settings 

  if (x$method %in% c("denton", "denton-cholette")){
    ext <- "('fitted.values': low-frequency indicator)"
  } else {
    ext <- "('fitted.values': low-frequency fitted values of the regression)"
  }
  
  subtitle <- paste("method:", x$method, ext)
  
  par(mfrow=c(2,1), mar = c(1.5, 4, 4, 2) + 0.1)                    
  ts.plot(ts.intersect(x$actual, x$actual - x$residuals),
          main=x$name, lty=c("solid", "dashed"), col=c("black", "red"), 
          ylab="actual and fitted.values (red)", ...); grid();
  mtext(subtitle, 3, line=.4, cex=.9)
  par(mar = c(4, 4, 1.5, 2) + 0.1) 
  ts.plot(ts.intersect(x$residuals, 0), lty=c("solid", "dashed"),
          col=c("black", "red"), ylab="residuals", ...); grid()
  on.exit(par(old.par))  # restore par settings
}


#' Predict Method for Temporal Disaggregation
#' 
#' Compute the disaggregated or interpolated (and extrapolated) high frequency
#' series of a temporal disaggregation.
#' 
#' @param object      an object of class \code{"td"}, usually, a result of a 
#'                    call to \code{\link{td}}.
#' @param \dots       further arguments passed to or from other methods.
#' 
#' @return \code{summary.td} returns a vector or a \code{"ts"} object, 
#'   containing the disaggregated or interpolated high frequency series of a
#'   temporal disaggregation.
#' 
#' @seealso \code{\link{td}} for the main function for temporal disaggregation.
#' @examples
#' data(swisspharma)
#' 
#' mod1 <- td(sales.a ~ imports.q + exports.q)
#' predict(mod1)
#' 
#' @keywords ts, models
#' @method predict td
#' @export
#' 
predict.td <- function(object, ...) {
  object$values
}
