#' Temporal Aggregation of Time Series (deprecated)
#' 
#' Function is deprecated and will be defunct soon. Use the more general convert function instead. See examples in ?convert.
#' 
#' @param x           a time series object of class \code{"ts"} or \code{"mts"}.
#' @param conversion  type of conversion: \code{"sum"}, \code{"average"}, 
#'                    \code{"first"} or \code{"last"}.
#' @param to          (low-frequency) destination frequency as a character 
#'                    string (\code{"annual"} or \code{"quarterly"}) or as a 
#'                    scalar (e.g. \code{1}, \code{2}, \code{4}).
#' @param ...         additional arguments, passed to the methods.
#'                    
#' @return \code{ta} returns an object of class \code{"ts"} or \code{"mts"}, 
#'   depending on the class of the input series.
#' 
#' @seealso \code{\link{td}} for the main function for temporal disaggregation.
#' @export
#' 
#' @examples
#' data(swisspharma)
#'   
#' sales.q.a <- ta(sales.q, conversion = "sum", to = "annual")
#' all.equal(sales.a, sales.q.a)
#' 
#' @keywords ts, models
ta <- function(x, ...) {
  .Deprecated("ta", package=NULL, "Function is deprecated and will be defunct soon. Use the more general convert function instead. See examples in ?convert.",
              old = as.character(sys.call(sys.parent()))[1L])
  
  convert(x, ...)
}


