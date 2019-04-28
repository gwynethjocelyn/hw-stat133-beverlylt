## Overview
The "`binomial`" package provides functions to further explore the binomial distribution.
- `bin_variable()` will list the parameters of the binomial distribution.
– `bin_choose()` will provide the number of combinations for a particular number of successes to occur in a set number of trials.
– `bin_probability()` will calculate the probability of getting the number of successes specified in the number of desired trials
– `bin_distribution()` will calculate probabilities of the number of successes in a fixed number of random trials performed under identical conditions
– `bin_cumulative()` calculates probability distribution and the cumulative probabilities
- `plot()` will plot the `bin_distribution()` histogram or plot the graph of binomial cumulative distribution of `bin_cumulative()`
- `summary()` will provide the summary of `bin_variable()` - where it will detail the parameters and measurements of a binomial distribution

## Additional Functions
The "`binomial`" package also includes other functionalities to examine the properties of the binomial distribution.
- `bin_mean()` will calculate the mean of the binomial distribution
- `bin_variance()` will calculate the variance of the binomial distribution
- `bin_mode()` will calculate the mode of the binomial distribution
- `bin_skewness()` will calculate the skewness of the binomial distribution
- `bin_kurtosis()` will calculate the kurtosis of the binomial distribution

## Motivation
This package is to discover and calculate the binomial distribution in a faster and more efficient way.

## Usage
```{r}
library(binomial)

#setting our binomial random variable
bin1 <- bin_variable(trials = 5, prob = 0.5)
bin1
```
```{r}
#a summary of our binomial random variable
summary(bin1)
```
```{r}
#the number of combinations where 2 successes can occur in 5 trials
bin_choose(n = 5, k = 2)
```
```{r}
#the binomial distribution
bin2 <- bin_distribution(trials = 10, prob = 0.3)
bin2
```
```{r}
#plotting the histogram of the bin2
plot(bin2)
```
```{r}
# Looking for measurements from a binomial distribution - the mean
bin_mean(trials = 6, prob = 0.3)
```

