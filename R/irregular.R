
CalcC_lf_hf <- function(lf, hf){
  hf.per.lf <- function(lf, hf){
    z <- integer(length(lf))
    for (i in 1:(length(lf) - 1)){
      z[i] <- sum((hf >= lf[i]) & (hf < lf[i + 1]))
    }
    z[length(z)] <- sum(hf >= lf[length(lf)])
    z
  }

  pp <- hf.per.lf(lf, hf)

  ll <- lapply(pp, function(e) rep(1, e))
  # this is 'sum', but we could do 'mean', 'first', and 'last' at this point

  z <- list()
  for (i in 1:length(lf)){
    l0 <- rep(list(0), length(lf))
    l0[[i]] <- ll[[i]]
    z[[i]] <- do.call(rbind, l0)
  }
  do.call(cbind, z)
}

# At least the Denton functions are perfectly able to work with this irregular 
# function, but I suspect the Chow-Lin functions as well.

# CalcC_lf_hf(lf, hf)