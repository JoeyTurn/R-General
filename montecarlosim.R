RNGkind(sample.kind ="Rounding")
set.seed(1)

gosset <- function(n, mean = 0, sd = 1) {
  testdata <- rnorm(n, mean, sd)
  tstattest <- mean(testdata)/(sd(testdata)/sqrt(5))
}
thou <- replicate(1000, gosset(5))
mean(thou > 2)

set.seed(1)
N <- 1500
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)

library(downloader)
library(readr)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

N=10
RNGkind(sample.kind = "Rounding")
set.seed(1)

nonsmokers <- sample(bwt.nonsmoke , 10)
smokers <- sample(bwt.smoke , 10)
obs <- median(smokers) - median(nonsmokers)

repitit <- replicate(1000, {dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
median(smokersstar)-median(nonsmokersstar)})
qqnorm(repitit)
(mean(repitit < obs))*2

