context("Checkers Function")

test_that("check_prob works as expected", {
  x <- 0.5
  y <- 1.3
  z <- c(0.5, 1)

  expect_length(check_prob(x), 1)
  expect_true(check_prob(x))
  expect_error(check_prob(z))
})

test_that("check_trials works as expected", {
  x <- 5
  y <- -4

  expect_true(check_trials(x))
  expect_length(check_trials(x), 1)
  expect_error(check_trials(y))
})

test_that("check_success works as expected", {
  x <- 3
  y <- c(1,2)
  z <- c(-1, 4)

  expect_length(check_success(y, x), 2)
  expect_error(check_success(z, x))
  expect_error(check_success(x, y))
})

context("Summary measures")

test_that("aux_mean works as expected", {

  expect_equal(aux_mean(10, 0.3), 3)
  expect_type(aux_mean(5, 0.5), 'double')
  expect_length(aux_mean(7,0.01), 1)

})

test_that("aux_variance works as expected", {

  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_type(aux_mean(5, 0.5), 'double')
  expect_length(aux_mean(7,0.01), 1)

})

test_that("aux_mode works as expected", {

  expect_equal(aux_mode(10, 0.3), 3)
  expect_type(aux_mode(5, 0.5), 'double')
  expect_length(aux_mode(7,0.01), 1)

})

test_that("aux_skewness works as expected", {

  expect_type(aux_skewness(5, 0.5), 'double')
  expect_length(aux_skewness(7,0.01), 1)
  expect_warning(aux_skewness(10, -0.3))

})

test_that("aux_kurtosis works as expected", {

  expect_type(aux_kurtosis(5, 0.5), 'double')
  expect_length(aux_kurtosis(7,0.01), 1)
  expect_equal(aux_kurtosis(10, 0), Inf)

})

context("Binomial Main Function")

test_that("bin_choose works as expected", {

  expect_error(bin_choose(3,4))
  expect_equal(bin_choose(5,1), 5)
  expect_length(bin_choose(5, 1:3), 3)
})

test_that("bin_probability works as expected", {

  expect_error(bin_probability(4, 3, 1))
  expect_type(bin_probability(3, 4, 1), 'double')
  expect_length(bin_probability(1:3, 5, 0.5), 3)
})

test_that("bin_distribution works as expected", {

  expect_error(bin_distribution(5, 1.2))
  expect_is(bin_distribution(5, 0.5), c("bindis", "data.frame"))
  expect_length(bin_distribution(5, 0.5), 2)
})

test_that("bin_cumulative works as expected", {

  expect_error(bin_cumulative(5, 1.2))
  expect_is(bin_cumulative(5, 0.5), c("bincum", "data.frame"))
  expect_length(bin_cumulative(5, 0.5), 3)
})
