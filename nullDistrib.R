library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
file <- read.csv(filename)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

RNGkind(sample.kind = "Rounding")
set.seed(1)

meanx <- mean(x)

n <- 1000
nulls <- vector("numeric", n)
for (i in 1:n) {
  meann <- mean(sample(x, 5))
  nulls[i] <- meann - meanx
}

mean(abs(nulls) > 1)
