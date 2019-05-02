#' @title Binomial Choose
#' @description calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#'
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n,k){
  ifelse(k > n, stop("k cannot be greater than n"), factorial(n)/(factorial(k)*factorial(n-k)))
}

#' @title Binomial Probability
#' @description calculates probability of getting a certain number successes in the number of desired trials
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @return probability of getting the number of successes specified in the number of desired trials
#' @export
#' @examples
#'
#' # probability of getting 2 successes in 5 trials
#  # (assuming prob of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' # probabilities of getting 2 or less successes in 5 trials
#  # (assuming prob of success = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#'
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob){
  ifelse(check_trials(trials) | check_prob(prob) | check_success(success, trials),
         bin_choose(trials, success) * prob^success * (1 - prob)^(trials-success),
         stop())
}

#' @title Binomial Distribution
#' @description calculate probabilities about the number of successes in a fixed number of random trials performed under identical conditions
#' @param trials number of trials
#' @param prob probability of success
#' @return data.frame with two classes: c("bindis", "data.frame")
#' @export
#' @examples
#'
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob){
  dist <- matrix(nrow = trials+1, ncol = 2)
  for(i in 0:trials){
    dist[i+1,1] = i
    dist[i+1,2] = bin_probability(success = 0:trials, trials = trials, prob = prob)[i+1]
  }
  dist <- as.data.frame(dist)
  names(dist) <- c("success", "probability")
  class(dist) <- c("bindis", "data.frame")
  dist
}

#' @export
plot.bindis <- function(x, ...) {
  barplot(x$probability, names.arg = x$success, main = "Binomial Probability Distribution",
          xlab = "successes", ylab = "probability")
}

#' @title Binomial Cumulative
#' @description calculates probability distribution and the cumulative probabilities
#' @param trials number of trials
#' @param prob probability of success
#' @return  a data frame with both the probability distribution and the cumulative probabilities: sucesses in the first column, probability in the second column, and cumulative in the third column
#' @export
#' @examples
#'
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob){
  table <- bin_distribution(trials = trials, prob = prob)
  table$cumulative <- cumsum(table$probability)
  class(table) <- c("bincum", "data.frame")
  table
}

#' @export
plot.bincum <- function(x, ...) {
  plot(x = x$success, y = x$cumulative , type = "o", main = "Binomial Cumulative Distribution",
       xlab = "successes", ylab = "probability")
}

#' @title Binomial Random Variable
#' @description lists the number of trials and probability of success
#' @param trials number of trials
#' @param prob probability of success
#' @return data.frame with two classes: c("bindis", "data.frame")
#' @export
#' @examples
#'
#' bin_variable(trials = 10, p = 0.3)
bin_variable <- function(trials, prob){
  if(check_trials(trials) | check_prob(prob)){
    result <- c(trials, prob)
    class(result) <- "binvar"
    names(result) <- c("trials", "prob")
    result
  } else{
    stop()
  }
}

#' @export
print.binvar <- function(x, ...) {
  cat('"Binomial Variable"\n\n')
  cat("Parameters\n")
  cat("- number of trials:", x["trials"], "\n")
  cat("- prob of success:", x["prob"], "\n")
}

#' @export
summary.binvar <- function(x, ...) {
  summ <- list(trials = x["trials"], prob = x["prob"], mean = aux_mean(x["trials"], x["prob"]),
               variance = aux_variance(x["trials"], x["prob"]), mode = aux_mode(x["trials"], x["prob"]),
               skewness = aux_skewness(x["trials"], x["prob"]), kurtosis = aux_kurtosis(x["trials"], x["prob"]))
  class(summ) <- "summary.binvar"
  summ
}

#' @export
print.summary.binvar <- function(x, ...){
  cat('"Summary Binomial"\n\n')
  cat("Parameters\n")
  cat("- number of trials:", x$trials, "\n")
  cat("- prob of success:", x$prob, "\n\n")
  cat("Measures\n")
  cat("- mean\t  :", x$mean, "\n")
  cat("- variance:", x$variance, "\n")
  cat("- mode\t  :", x$mode, "\n")
  cat("- skewness:", x$skewness, "\n")
  cat("- kurtosis:", x$kurtosis, "\n")
}
