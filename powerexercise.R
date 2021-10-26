library(downloader)
library(readr)
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

RNGkind(sample.kind = "Rounding")
set.seed(1)

N <- 5
reject <- function(N, alpha=.01) {
  ns <- sample(bwt.nonsmoke, N)
  s <- sample(bwt.smoke, N)
  pval <- t.test(ns, s)$p.value
  pval < alpha
}

rejects <- replicate(10000, reject(90))
mean(rejects)
