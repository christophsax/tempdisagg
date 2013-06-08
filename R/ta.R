ta <- function(x, conversion = "sum", to = "annual"){
  # performs a temporal agregation of one or several time series (main function)
  #
  # Args:
  #   x:            a time series object of class "ts" or "mts"
  #   conversion:   type of conversion ("sum", "average", "first", "last")
  #   to:           destination frequency ("quarterly" or "monthly"), only for
  #                 Denton without indicators
  #
  # Returns:
  #   An object of class "ts" or "mts", depending on the class of the input series.
  #
  # Remarks:
  #   Calls SubAggregation for computation

  if (is.numeric(to)){  # frequency specified by a number
    f_l <- to
  } else if (is.character(to)){  # frequency specified by a char string
    if (to=="annual"){
      f_l <- 1
    } else if (to=="quarterly"){
      f_l <- 4
    } else {
      stop("unknown character string as the 'to' argument")
    }
  } else stop ("wrong specification of the 'to' argument")

  if (!inherits(x, "ts")) stop("not a time series object.")

  if (inherits(x, "mts")){
    ncol  <- dim(x)[2]
    first  <- SubAggregation(x[,1], conversion=conversion, f_l=f_l)
    mat <- matrix(NA, nrow=length(first), ncol=ncol)
    for (i in 1:ncol) {
      mat[,i] <- SubAggregation(x[,i], conversion=conversion, f_l=f_l)
    }
    z <- ts(mat, start=start(first), frequency=f_l)
    dimnames(z)[2] <- dimnames(x)[2]
  } else {
    z <- SubAggregation(x, conversion=conversion, f_l=f_l)
  }
  z
}


SubAggregation <- function(x, conversion = "sum", f_l = 1){
  # performs a temporal agregation of a single time series
  #
  # Args:
  #   x:            a single time series object of class "ts"
  #   f_l:          frequency of the (low-frequency) destination series. Overrides the to argument. 
  #   to:           destination frequency ("quarterly" or "monthly"), only for Denton without indicators
  #
  # Returns:
  #   A time series object of class "ts"

  f <- frequency(x)
  fr <- f / f_l

  hf.start <- time(x)[!is.na(x)][1]
  hf.start.na <- time(x)[1]
  hf.end <- tail(time(x)[!is.na(x)],1)
  hf.end.na <- tail(time(x),1)

  lf.start <- SubConvertStart(hf.start = hf.start, f = f, f_l = f_l)
  lf.start.na <- SubConvertStart(hf.start = hf.start.na, f = f, f_l = f_l)

  lf.end <- SubConvertEnd(hf.end = hf.end, f = f, f_l = f_l)
  lf.end.na <- SubConvertEnd(hf.end = hf.end.na, f = f, f_l = f_l)

  # if series contains only NAs, return NAs
  if (all(is.na(x))){
    z <- window(ts(NA, start=lf.start.na, frequency=f_l), end=lf.end.na, extend=TRUE)
  } else {
    x.used <- window(x, start=lf.start, end=lf.end + 1 / f_l - 1/ f)
    agg <- as.numeric(CalcC(n_l=length(x.used)/fr, conversion=conversion, fr=fr) %*% x.used)
    agg.ts <- ts(agg, start=lf.start, frequency=f_l)
    z <- window(agg.ts, start=lf.start.na, end=lf.end.na, extend=TRUE)
  }

  z
}


SubConvertEnd <- function(hf.end, f, f_l){
  # converts a hf end point to the last fully available lf end point
  #
  # Args:
  #   hf.end:       a scalar indicating the time stamp of the last available 
  #                   high frequency obs.
  #   f:            frequency of the high-frequency series
  #   f_l:          frequency of the low-frequency series
  #
  # Returns:
  #   a scalar indicating the time stamp of last complete low frequency value
  #
  # Remarks:
  #   Identical to SubConvertStart() except that ceiling() is exchanged by
  #   floor()

  fr <- f / f_l
  floor(hf.end) + (floor(((hf.end - floor(hf.end)) * f + 1 + 1e-8) / fr) - 1) / f_l
  # +1e-8 avoids rounding problems
}


SubConvertStart <- function(hf.start, f, f_l){
  # converts a hf end point to the last fully available lf end point
  #
  # Args:
  #   hf.start:     a scalar indicating the time stamp of the first available
  #                   high frequency series
  #   f:            frequency of the high-frequency series
  #   f_l:          frequency of the low-frequency series
  #
  # Returns:
  #   a scalar indicating the time stamp of last complete low frequency value
  #
  # Remarks:
  #   Identical to SubConvertEnd() except that floor() is exchanged by ceiling().

  fr <- f / f_l
  floor(hf.start) + ceiling(((hf.start - floor(hf.start)) * f)/fr - 1e-8)/f_l
  # -1e-8 avoids rounding problems
}

