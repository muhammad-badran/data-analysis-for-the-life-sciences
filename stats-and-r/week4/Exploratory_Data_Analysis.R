# install.packages("UsingR")

library(UsingR)
x=father.son$fheight

#How many individuals we have?
length(x)

# take sample of 20 from the 1078 data points
# round the the numbers to the nearest of 10th of an inch
round(sample(x,20), 1)

## ------------------------------------------------------------------

# visual description of the data
hist(x, breaks=seq(floor(min(x)),ceiling(max(x))), main="Height histogram",
     xlab="Height (inches)")

## ------------------------------------------------------------------

# the empirical commutative distribution function
# it reports for any given number the percent of individuals that are below
# that threshold.
xs <- seq(floor(min(x)), ceiling(max(x)), 0.1)
plot(xs, ecdf(x)(xs), type="l", xlab="Height (inches)", ylab="F(X)")

## ------------------------------------------------------------------
# after we know from histogram that distribution is normal
# all we need to summarize it is mean and the standard deviation
# the mean of height above 70 inches is near to 20%
mean(x>70)
1-pnorm(70, mean(x), sd(x))

# we can reverse the above operation (less than 70 inches)(80%)
mean(x<70)
pnorm(70, mean(x), sd(x))

# if we change the value of 70 inches to other number (ex. 69) and the two 
# values are close then we can approximate to a normal distribution
mean(x>69)
1-pnorm(69, mean(x), sd(x))

## ------------------------------------------------------------------


# instead of trying many numbers we can use QQ plot (Quantile-Quantile plot)
ps <- seq(0.01, 0.99, 0.01)
qs <- quantile(x, ps) # observed percentiles
nomralqs <- qnorm(ps, mean(x), sd(x)) # predicted percentile
plot(nomralqs, qs, xlab="Normal Percentiles", ylab="Height Percentile")
abline(0,1) # identity line

## ------------------------------------------------------------------

# to do the above in easier and direct way use qqnorm(y)
qqnorm(x)
qqline(x)

## ------------------------------------------------------------------
# to deal with NOT normally distributed data
# the mean and standard deviation are NOT necessarily a good summary
# salary data
hist(exec.pay)
qqnorm(exec.pay) # we have "fat right tail"
qqline(exec.pay)

## ------------------------------------------------------------------

boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400))
# mean and median are not close
mean(exec.pay)
median(exec.pay)

## ------------------------------------------------------------------

data("father.son")
x=father.son$fheight
y=father.son$sheight
plot(x,y,xlab="Father's height (inches)", ylab = "Son's height (inches)")

## ------------------fig.width=6, fig.height=3------------------------
# stratifying the data
# rounding the father's height and group by that value using split
boxplot(split(y,round(x)))
print(mean(y[round(x)==72]))

## ------------------fig.width=6, fig.height=3------------------------
# to get correlation:
# standardize the data -> multiply them -> divided by the mean
x_scaled = (x-mean(x))/sd(x)
y_scaled = (y-mean(y))/sd(y)

means=tapply(y_scaled, round(x_scaled*4)/4, mean)
fatherheights=as.numeric(names(means))

plot(fatherheights, means, ylab="average starta of son heights",
     ylim=range(fatherheights))
abline(0,cor(x,y))


## ------------------fig.width=3, fig.height=3------------------------
# generating uncorrelated data
set.seed(1)
a = rnorm(100);a[1]=25
b = rnorm(100);b[1]=26
plot(a,b,main=paste("correlation =", signif(cor(a,b),2)))
cor(a,b,method="spearman")

## -------------------------------------------------------------------

# In exponential functions, ratios are not symmeteric around 1
# Log ratios are symmeteric around 0

# log(x/y) = log(x) - log(y) = -(log(y) - log(x)) = -log(y/x)

## -------------------------------------------------------------------

# How to display data badly - by karl Broman
# https://github.com/kbroman/Talk_Graphs

# plots to avoid
# -----------------

# bar plot is easier to read than pie chart
# bar plots is affected by outliers and incorrect scales (use log scales instead)

# sometimes comparing pairs using scatter plot is better than bar plot
# use precision when you work with floating point numbers to represent data clearly
  
# avoid pseudo-3d

# --------------------------------------------
# summary statistics that is robust to outliers
# Medianc
# MAD (Median Absolute Deviation) is a robust estimate of the standard deviation
# 1.4826 median{|xi - median(xi)|}
# We also use Spearman correlation: instead of look at the values we look at the ranks

# Mann-Whitney-Wilcoxon Test
# ---------------------------

# z score

# (U/n1 - n2/2) / sqrt(n2(n1+n2+1)/12n1)
