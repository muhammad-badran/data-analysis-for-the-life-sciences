library(dplyr)

data <- read.csv("mice_pheno.csv")

controlPopulation <- filter(data, Sex =="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist

hfPopulation <- filter(data, Sex =="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist

mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)

print(mu_hf - mu_control)
print((mu_hf - mu_control)/mu_control * 100)

set.seed(1)

N <- 5
hf <- sample(hfPopulation, N)
control <- sample(controlPopulation, N)
t.test(hf,control)$p.value

N <- 12
alpha <- 0.05 # the cutoff at which we reject the hypothesis if p-value is smaller
B <- 2000

reject <- function(N, alpha=0.05) {
  hf <- sample(hfPopulation, N)
  control <- sample(controlPopulation, N)
  pval <- t.test(hf, control)$p.value
  pval < alpha
}

reject(12) # see if we can reject or not

# use replicated to do it B=2000 times
rejections <- replicate(B, reject(N))

mean(rejections)

Ns <- seq(5, 50, 5) # several values of N

power <- sapply(Ns, function(N){
  rejections <- replicate(B, reject(N))
  mean(rejections)
})

plot(Ns, power, type="b")
