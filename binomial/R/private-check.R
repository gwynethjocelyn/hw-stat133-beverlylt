# private checker function to check to test if an input prob is a valid probability value (i.e. 0 ≤ p ≤ 1)
check_prob <- function(prob) {
  if (length(prob) != 1 | !is.numeric(prob)) {
    stop("\ninvalid prob value")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\np has to be a number betwen 0 and 1")
  }
  TRUE
}

# private checker function to check to test test if an input trials is a valid value for number of trials (i.e. n is a non-negative integer)
check_trials <- function(trials) {
  if (trials < 0 | trials%%1 != 0) {
    stop("\ninvalid trials value")
  }
  TRUE
}

# private checker function to check to test  if an input success is a valid value for number of successes (i.e. 0 ≤ k ≤ n).
check_success <- function(success, trials) {
  ifelse((success%%1 != 0 | success < 0), stop("\ninvalid success value"), TRUE)
  ifelse(success > trials, stop("\nsuccess cannot be greater than trials"), TRUE)
}
