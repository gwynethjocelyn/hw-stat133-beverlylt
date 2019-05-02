# private auxiliary function to calculate mean
aux_mean <- function(trials, prob){
  trials * prob
}

# private auxiliary function to calculate variance
aux_variance <- function(trials, prob){
  trials * prob * (1-prob)
}

# private auxiliary function to calculate mode
aux_mode <- function(trials, prob){
  result <- trials * prob + prob
  if(result%%1 == 0){
    c(result, result-1)
  }
  else{
    floor(result)
  }
}

# private auxiliary function to calculate skewness
aux_skewness <- function(trials, prob){
  num <- 1 - 2*prob
  den <- aux_variance(trials,prob)
  num/sqrt(den)
}

# private auxiliary function to calculate kurtosis
aux_kurtosis <- function(trials, prob){
  num <- 1 - 6*prob*(1-prob)
  den <- aux_variance(trials, prob)
  num/den
}
