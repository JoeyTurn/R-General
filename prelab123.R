library(readr)
library(downloader)
library(dplyr)

data("cars")
names(cars)
dim(cars)

#dist(t(cars))
#image(as.matrix(dist(t(cars))))

x <- cars$speed
hist(x, breaks = 30)

read.csv("", header = T)
par(mfrow = c(3, 3))



###
#Pre-lab 3
x <- c(start:end)

binom_values <- dbinom(x, n, p)
plot(x, binom_values, type = "b")
poisson_values <- dpois(x, lambda = lambda)
par(mfrow = c(1,1))
plot(x, poisson_values, type = "b")
lines(x, poisson_values, type = "b", col = 2, main = 'lambda = lambda')

rbinom(n, size, prob) #generates n numbers from the binom distrib with size = size and prob = p
rbin <- rbinom(100, 10, .5)
rpois(n, lambda) #generates n numbers from the poiss distrib with lambda = lambda
rnorm(n, mu, sigma) #generates n numbers from the norm distrib with mu = mu and sigma = sigma

boxplot(rnorm(10000), cex = .70) #to hide outliers, use range = 0

#cex changes the size of the circles

#overlaying histograms

x <- rnorm(100, 1, 1)
y <- rnorm(100, 1, 1)
hist(x, breaks = 20)
hist(y, breaks = 20, add = T, border = 2)

#sample quantiles

median(x)
sort(x)
quantile(x, probs = .5) #same as median, can change probs

#finding quantiles
qnorm(p, mean, sd, lower.tail = TRUE) #finds the pth percentile, includes everything below that point
qnorm(.90, lower.tail = TRUE)

n <- 500
x <- rnorm(n)
qqnorm(x, cex = .8)
abline(0, 1)

#p and q are essentially opposites

