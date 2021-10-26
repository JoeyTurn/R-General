install.packages("rafalib")
library(rafalib)
library(downloader) 
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 
dat <- na.omit( dat )
head(dat)

x <- dat %>%
  filter(Sex == "F", Diet == "chow") %>%
  select(Bodyweight) %>%
  unlist

y <- dat %>%
  filter(Sex == "F", Diet == "hf") %>%
  select(Bodyweight) %>%
  unlist

mX <- mean(x)
rafalib::popsd(x)

RNGkind(sample.kind = "Rounding")
set.seed(1)

mx <- mean(sample(x, 25))

mY <- mean(y)
rafalib::popsd(y)
my <- mean(sample(y, 25))

abs((my-mx)-(mY-mX))
