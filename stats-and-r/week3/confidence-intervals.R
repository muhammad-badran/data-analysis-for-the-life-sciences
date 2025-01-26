# confidence intervals

set.seed(1)

chowPopulation <- read.csv("femaleControlsPopulation.csv")
chowPopulation <- unlist(chowPopulation)

mu_chow <- mean(chowPopulation)

print(mu_chow)

N <- 30 # sample size

chow <- sample(chowPopulation, N)
print(mean(chow))

se <- sd(chow)/sqrt(N)
print(se)

(mean(chow) - mean(chowPopulation)) / se
pnorm(2) - pnorm(-2) # it supposed to be 1.95, we wrote 2 for approximation

Q <- qnorm(1 - 0.05/2)
interval <- c(mean(chow) - Q*se, mean(chow) + Q*se )
interval

library(rafalib)
B <- 250

mypar()
plot(mean(chowPopulation) + c(-7,7), c(1,1), 
     type="n", xlab="weight", ylab="interval", ylim=c(1,B))

abline(v=mean(chowPopulation))

for (i in 1:B) {
  chow <- sample(chowPopulation, N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se )
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col=color)
}


mypar()
plot(mean(chowPopulation) + c(-7,7), c(1,1),
     type="n", xlab="weight", ylab="interval", ylim=c(1,B))

abline(v=mean(chowPopulation))

Q <- qnorm(1- 0.05/2)
N <- 5
## many intervals are red (bad)
for(i in 1:B) {
  chow <- sample(chowPopulation, N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se )
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col=color)
}

################ No longer normal distribution we use t distribution instead

mypar()
plot(mean(chowPopulation) + c(-7,7), c(1,1),
     type="n", xlab="weight", ylab="interval", ylim=c(1,B))

abline(v=mean(chowPopulation))

# Q <- qnorm(1- 0.05/2) # no longer normal distribution
Q <- qt(1-0.05/2, df=4) # t distribution
N <- 5

for(i in 1:B) {
  chow <- sample(chowPopulation, N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow) - Q*se, mean(chow) + Q*se )
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered, 1, 2)
  lines(interval, c(i,i), col=color)
}

