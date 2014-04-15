#' General Frequency Conversion
#' 
#' Converts the frequency of a time series object to any other frequency. 
#' Performs temporal disaggregation if the target frequency is higher and
#' temporal aggregation if the target frequency is lower.
#' 
#' Preserves NA values at the beginning and at the end. Internal NAs result in a series that only contains NAs (see examples).
#' 
#' @param x  an object to temporally convert, usually an object of class 
#'   \code{"ts"} or \code{"mts"}.
#' @param to  destination frequency as a character string ("annual", "quarterly"
#'   or "monthly") or as a scalar (e.g. 2, 4, 7, 12).
#' @param conversion  type of temporal conversion: \code{"sum"}, 
#'   \code{"average"}, \code{"first"} or \code{"last"}.
#' @param method      method of temporal disaggregation without an indicator: 
#'   \code{"denton-cholette"}, \code{"denton"}, \code{"uniform"} or 
#'   \code{"ols"}. Only has an effect if destination frequency is higher. See 
#'   \code{\link{td}} for details.
#' @return an object of the same class as \code{x}, with frequency converted. If
#'   the destination frequency is equal to the orginal frequency, the input is
#'   returned.
#' @export
#' @examples
#' convert(cbind(mdeaths, fdeaths), to = 1)  # handling "mts" objects
#' convert(austres, to = 12)                 # disaggregation
#' convert(austres, to = 1)                  # aggregation
#' 
#' # NAs are perserved, incomplete periods are omitted
#' convert(window(mdeaths, end = c(1982, 5), extend = TRUE), to = 1)
#' 
#' airmiles.q <- convert(airmiles, to = 4)
#' all.equal(convert(airmiles.q, to = 1), airmiles)
#' 
convert <- function(x, to = "quarterly", conversion = "sum", method = "denton-cholette", ...) UseMethod("convert")

#' @export
#' @method convert ts
convert.ts <- function(x, to = "quarterly", conversion = "sum", method = "denton-cholette", ...) {
  stopifnot(inherits(x, "ts"))
  
  if (is.character(to)) {
    if (to == "annual") {
      to <- 1
    } else if (to == "quarterly") {
      to <- 4
    } else if (to == "monthly") {
      to <- 12
    } else {
      stop("'to' argument: unknown character string")
    }
  }
  
  if (is.null(dim(x))){
    n <- 1
  } else {
    n <- dim(x)[2]
  }
  
  y <- list()
  for (i in 1:n){
    # distinguish single ts and mts
    if (n == 1){
      xi.na <- x
    } else {
      xi.na <- x[, i]
    }
    
    if (frequency(x) < to){
      # if NA handling in td would be proper, this would be unncessary
      xi <- na.omit(xi.na)
      dummy.ts <- predict(td(xi.na ~ 1, to = to, conversion = conversion, method = method, ...))
      
      yi <- predict(td(xi ~ 1, to = to, method = method, ...))
      y[[i]] <- window(yi, start = start(dummy.ts), end = end(dummy.ts), extend=TRUE)
    } else if (frequency(x) > to){
      y[[i]] <- SubAggregation(xi.na, to = to, conversion = conversion, ...)
    } else {
      y[[i]] <- xi.na
    }
  }
  z <- do.call(cbind, y)
  dimnames(z)[2] <- dimnames(x)[2]
  z
}
