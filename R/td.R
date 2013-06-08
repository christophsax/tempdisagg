td <- function(formula, conversion = "sum", to = "quarterly", 
               method = "chow-lin-maxlog", neg.rho = FALSE, fixed.rho = 0.5, 
               criterion = "proportional", h = 1,
               start = NULL, end = NULL, ...) {
  # main function of temporal disaggregation or interpolation
  #
  # Args:
  #   formula:      a formula specifying the temporal disaggregation model
  #   conversion:   type of conversion ("sum", "average", "first", "last")
  #   to:           destination frequency ("quarterly" or "monthly"), or scalar
  #                 necessary for Denton type methods without indicators only
  #   method:       method of temporal disaggregation
  #   neg.rho:      should a negative rho (AR1-parameter) be allowed 
  #   fixed.rho:    set a predefined rho
  #   start:        optional starting point of the disaggregation
  #   end:          optional end point
  #   ...           Further arguments, passed on to the subfunctions
  #
  # Returns:
  #   An object of class "td", containing the output of the subroutines and 
  #     the following elements:
  #   method        method of temporal disaggregation
  #   call          function call
  #   fr            frequency ratio between the low- and high-frequency series
  #   conversion    type of conversion
  #   actual        actual values of the low-frequency series
  #   model         a matrix containing the indicators (and an intercept if
  #                 present)
  #
  # Remarks:
  #   the function deals with the formula interface, the time-series properties
  #   and allows for optional shortening of the series. The estimation itself is
  #   done by subfunctions starting with Sub...
  
  cl <- match.call()
  
  
  # --- input consistency ----------------------------------------------------
  
  # dont allow length=2 vectors as start or end inputs
  if (any(c(length(start), length(end)) > 1)){
    stop("'start' or 'end' must be specified as a decimal fraction")
  }
  
  
  # ---- prepare Formula, extract names and data ------------------------------
  
  # extract X (right hand side, high frequency) formula, names and data
  X.formula <- formula; X.formula[[2]] <- NULL
  X.series.names <- all.vars(X.formula)
  
  # extract y_l (left hand side, low frequency) formula, values and names
  y_l.formula <- formula[[2]]
  y_l.series <- eval(y_l.formula, envir=environment(formula))
  y_l.name <- deparse(y_l.formula)
  
  
  # ---- set ts.mode ----------------------------------------------------------
  
  # 1. is y_l.series a time series? if so, set ts.mode to TRUE
  if (is.ts(y_l.series)){
    ts.mode <- TRUE
    # 2. is there a X? is it a time series? if not, set ts.mode to FALSE
    if (length(X.series.names) > 0) {
      if (!is.ts(get(X.series.names[1], envir=environment(X.formula)))){
        warning("Only left hand side is a time series. Using non-ts mode.")
        ts.mode <- FALSE
      }
    }
  } else{
    ts.mode <- FALSE
  }
  
  
  # ---- set or modify time series attributes ('fr', 'start', 'end') ----------
  
  if (ts.mode) {
    
    ### ts.mode: y_l.series
    if (is.null(start)) {
      start <- time(y_l.series)[!is.na(y_l.series)][1]
    }
    f_l <- frequency(y_l.series)
    
    ### ts.mode: X.series
    if (length(X.series.names) > 0){  # If X is pecified
      # first series of X (to get 'start' and 'f')
      X.series.first <- eval(X.formula[[2]], envir = environment(X.formula))
      X.start <- time(X.series.first)[!is.na(X.series.first)][1]
      f <- frequency(X.series.first)
      fr <- f/f_l
      if (X.start > start){
        start <- floor(X.start) + 
          (ceiling(((X.start - floor(X.start)) * f) / fr)) / f_l
      }
      
    } else {  # If no X is specified
      if (is.numeric(to)){  # frequency specified by a number
        f <- to
      } else if (is.character(to)){  # frequency specified by a char string
        if (to == "quarterly"){
          f <- 4
        } else if (to == "monthly"){
          f <- 12
        } else {
          stop("'to' argument: unknown character string")
        }
      } else stop ("'to' argument: wrong specification")
      fr <- f/f_l
      X.start <- start
    }
    
    # define X.end, if 'end' is specified
    if(!is.null(end)){
      X.end <- floor(end) + (fr * ((end-floor(end)) * f_l + 1) - 1) / f
    } else {
      X.end <- NULL
    }
  }  else {
    
    ### non ts.mode
    if (!is.numeric(to)){stop("In non-ts mode, 'to' must be an integer number.")}
    f_l <- 1
    f <- to
    fr <- f/f_l
  }
  
  # --- raw X matrix ----------------------------------------------------------
  
  if (length(X.series.names) > 0){
    X <- model.matrix(X.formula)
    X.names <- dimnames(X)[[2]]
  } else {  
    # if there is no X Variables, set it to a constant ('Denton' Methods)
    X <- matrix(rep(1, times = length(y_l.series) * fr))
    if (!(method %in% c("denton-cholette", "denton", "uniform"))) {
      warning ("No indicator specified: denton,
               denton-cholette or uniform are recommended.")
    }
    X.names <- "(Intercept)"
    }
  
  if (ts.mode){
    X <- ts(X, start = X.start, frequency = f)
  }
  
  # --- adjust length of y_l.series and X (only ts.mode) -----------------------
  
  if (ts.mode){
    y_l.series <- window(y_l.series, start = start, end = end)
    X <- window(X, start = start, end = X.end)
  }
  
  
  # --- final data matrices ---------------------------------------------------
  
  y_l <- as.matrix(y_l.series)
  X <- matrix(X, nrow=nrow(X), ncol=ncol(X))
  dimnames(X) <- list(NULL, X.names)
  
  
  # --- estimation ------------------------------------------------------------
  
  # actual estimation 
  if (method %in% c("chow-lin-maxlog", "chow-lin-minrss-ecotrim",
                    "chow-lin-minrss-quilis", "chow-lin-fixed",
                    "litterman-maxlog", "litterman-minrss", "litterman-fixed", 
                    "fernandez", "ols")){
    z <- SubRegressionBased(y_l = y_l, X = X, conversion = conversion, 
                            method = method, neg.rho = neg.rho, 
                            fixed.rho = fixed.rho, fr = fr, ...)
  } else if (method %in% c("denton-cholette", "denton", "uniform")){
    z <- SubDenton(y_l = y_l, X = X, conversion = conversion, method = method, 
                   criterion = criterion, h = h, fr = fr, ...)
  } else {
    stop("method does not exist")
  }
  
  
  # --- output ----------------------------------------------------------------
  
  # add coefficent names to output
  if (!is.null(z$coefficients)) {
    names(z$coefficients) <- names(z$se) <- X.names
  }
  
  # additional output
  z$method             <- method
  z$call               <- cl
  z$name               <- y_l.name
  z$fr                 <- fr
  z$conversion         <- conversion
  z$actual             <- y_l.series
  z$model              <- X
  if (ts.mode) {
    z$model            <- ts(z$model, start = start, frequency = f)
    z$p                <- ts(z$p, start = start, frequency = f)
    z$fitted.values    <- ts(z$fitted.values, start = start, frequency = f)
    z$residuals        <- ts(z$residuals, start = start, frequency = f_l)
    z$actual           <- ts(z$actual, start = start, frequency = f_l)
  }
  class(z) <- "td"
  z
}
