# Random-variables-and-central-limit-theorem

library("dplyr")

data <- read.csv('datasets/femaleMiceWeights.csv')

control <- filter(data, Diet=="chow") %>% select(Bodyweight) %>% unlist

# high fat diet
treatment <- filter(data, Diet=="hf") %>% select(Bodyweight) %>% unlist

# mean difference
obs <- mean(treatment) - mean(control)

# data frame
population <- read.csv("datasets/femaleControlsPopulation.csv")

# converting data frame to numeric vector using unlist
population <- unlist(population)

mean(sample(population, 12))

# create control and treatment samples from populations
# to test for the null hypothesis
control_sample <- sample(population, 12)
treatment_sample <- sample(population, 12)
mean(treatment_sample) - mean(control_sample)
# every time you run the previous three lines of code
# gives new mean

# iterations to run them many times
iterations <- 10000
nulls <- vector("numeric", iterations)
for(i in 1:iterations) {
  control_sample <- sample(population, 12)
  treatment_sample <- sample(population, 12)
  nulls[i] <- mean(treatment_sample) - mean(control_sample)
}

max(nulls)

# null distribution
hist(nulls)

# get how many times the null is greater than the observed mean
mean( nulls > obs) # equivalent to: sum( null > obs) / iterations

mean( abs(nulls) > obs)

# The p-value is the answer to the question:
# what is the probability that an outcome from the null
# distribution is bigger than what we observed when the
# null hypothesis is true. (2.51%)

library("rafalib")

mypar()

qqnorm(nulls)
qqline(nulls)


############## t-test ##############

control <- filter(data, Diet=="chow") %>% select(Bodyweight) %>% unlist

treatment <- filter(data, Diet=="hf") %>% select(Bodyweight) %>% unlist

N <- length(treatment)
obs <- mean(treatment) - mean(control)

se <- sqrt(
  var(treatment)/N + var(control)/N
)

tstat <- obs/se

# to get the p-value subtart 1 - pnorm(tstat)
# to get the absolute value multiply by 2 (both tails)
2* (1 - pnorm(tstat))


# to verify use population data 
# (we usually do not have population data)

n <- 10000
nulls <- vector("numeric", N)
for(i in 1: n){
  control <- sample(population, N)
  treatment <- sample(population, N)
  se <- sqrt(
    var(treatment)/N + var(control)/N
  )
  nulls[i] <- mean(treatment) - mean(control)
}

mypar()
qqnorm(nulls)
abline(0,1)

# by replacing the sample size to 3 instead of N=12
# the normal approximation is not good at all with this dataset
# bigger sample sizes are better (30 rule of thumb)

ttest <- t.test(treatment, control)
ttest

# the difference in p-value is because here we are not
# assuming that the central limit theorem applies.
# Instead we use t-distribution approximation which has
# longer tails (smaller p-value)

qqnorm(control)
qqline(control)

qqnorm(treatment)
qqline(treatment)
