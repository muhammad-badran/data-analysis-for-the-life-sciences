# Monte Carlo simulation is generating random variables using computers

seed.seed(1)

library(rafalib)

data <- read.csv("mice_pheno.csv")

controlPopulation <- read.csv("femaleControlsPopulation.csv")
controlPopulation <- unlist(controlPopulation)

ttestgenerator <- function(n) {
  # note that here we have a false "high fat" group where we actually
  # sample from the nonsmokers. This is because we are modeling the *null*
  
  cases <- sample(controlPopulation, n)
  controls <- sample(controlPopulation, n)
  tstat <- (mean(cases)-mean(controls)) / 
    sqrt(var(cases)/n + var(controls)/n)
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(10))

hist(ttests)

qqnorm(ttests)
abline(0,1)

# CLT works well with this data set when the sample size is 10. Let's try size of 3.

ttests <- replicate(1000, ttestgenerator(3))
qqnorm(ttests)
abline(0,1)

# we see that when the size is 3 the approximation is not that good.

# Let's see how well t-distribution approximates the results
ps <- (seq(0,999)+0.5)/1000
qqplot(qt(ps, df=2*3-2), ttests, xlim=c(-6,6), ylim=c(-6,6))
abline(0,1)

qqnorm(controlPopulation)
qqline(controlPopulation)

# we can generate a control generation using parametric monte carlo
controls <- rnorm(5000, mean=24, sd=3.5)

# creating new function
ttestgenerator <- function(n, mean=24, sd=3.5) {
  cases <- rnorm(n,mean,sd) # instead of taking a sample, we generate
  #  a normally distributed random variable
  controls <- rnorm(n,mean, sd)
  tstat <- (mean(cases)-mean(controls)) /
    sqrt (var(cases)/n + var(controls)/n)
  return(tstat)
}

ttests <- replicate(1000, ttestgenerator(3))

qqnorm(ttests)
abline(0,1)
