#' Methods for Temporal Disaggregation and Interpolation of Time Series
#' 
#' @description Temporal disaggregation methods are used to disaggregate and
#'   interpolate a low frequency time series to a higher frequency series. This
#'   can be done without additional information or by exploiting the information
#'   contained in one or more indicators. All disaggregation methods ensure that
#'   either the sum, the average, the first or the last value of the resulting
#'   high frequency series is consistent with the low frequency series.
#'   
#'   The best way to start is to run the interactive demo:
#'   
#'   \code{demo(tempdisagg)}
#'   
#' @name tempdisagg-package
#' @aliases tempdisagg
#' @docType package
#' @author Christoph Sax \email{christoph.sax@@gmail.com}, Peter Steiner
#' @keywords package
#' @seealso \code{\link{td}} for more information on usage.
NULL


#' Trade and Sales of Chemical and Pharmaceutical Industry
#' 
#' This data set contains the monthly and quarterly imports and exports of the
#' chemical and pharmaceutical industry in Switzerland (in Mio. Swiss Francs) as
#' well as their quarterly and annual sales (Index).
#' 
#' @docype data
#' 
#' @format Each time series is an object of class \code{"ts"}. The number of
#' observations depends on the frequency.
#' @source Import and Export Data are from the Swiss Federal Customs 
#' Administration. Sales Data are from the Swiss Federal Statistical Office.
#' 
#' @name exports.m
#' @aliases exports.q imports.q sales.a sales.q
#' @keywords datasets
NULL