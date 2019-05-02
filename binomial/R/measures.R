#' @title Binomial Mean
#' @description calculates mean of binomial ditribution
#' @param trials number of trials
#' @param prob probability of success
#' @return mean of binomial ditribution
#' @export
#' @examples
#'
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob){
  if(check_trials(trials) | check_prob(prob)){
    aux_mean(trials, prob)
  }
  else{
    stop()
  }
}

#' @title Binomial Variance
#' @description calculates variance of binomial ditribution
#' @param trials number of trials
#' @param prob probability of success
#' @return variance of binomial ditribution
#' @export
#' @examples
#'
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob){
  if(check_trials(trials) | check_prob(prob)){
    aux_variance(trials, prob)
  }
  else{
    stop()
  }
}

#' @title Binomial Mode
#' @description calculates mode of binomial ditribution
#' @param trials number of trials
#' @param prob probability of success
#' @return mode of binomial ditribution
#' @export
#' @examples
#'
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob){
  if(check_trials(trials) | check_prob(prob)){
    aux_mode(trials, prob)
  }
  else{
    stop()
  }
}

#' @title Binomial Skewness
#' @description calculates skewness of binomial ditribution
#' @param trials number of trials
#' @param prob probability of success
#' @return skewness of binomial ditribution
#' @export
#' @examples
#'
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob){
  if(check_trials(trials) | check_prob(prob)){
    aux_skewness(trials, prob)
  }
  else{
    stop()
  }
}

#' @title Binomial Kurtosis
#' @description calculates kurtosis of binomial ditribution
#' @param trials number of trials
#' @param prob probability of success
#' @return kurtosis of binomial ditribution
#' @export
#' @examples
#'
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob){
  if(check_trials(trials) | check_prob(prob)){
    aux_kurtosis(trials, prob)
  }
  else{
    stop()
  }
}
