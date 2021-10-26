library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

RNGkind(sample.kind = "Rounding")
set.seed(1)

n <- 100
p <- 1/6
se <- p*(1-p)/n
z <- replicate(10000, (mean((sample(1:6, n, replace=TRUE))==6) - p)/sqrt(se))
z
mean(z)
var(z)
tstat <- t.test(z)
mean(abs(z) > 2)
?pnorm
mypar()
qqline(z)
qqnorm(z)

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
Xbar <- mean(X)
Ybar <- mean(Y)

2 * ( 1-pnorm(2/sd(X) * sqrt(12) ) )

xytest <- sqrt(var(X)/12 + var(Y)/12)
xy <- (Xbar-Ybar)/xytest
pnorm(xy)

t.test(X, Y)
