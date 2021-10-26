library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
file <- read.csv("msleep_ggplot2.csv")
download(url,filename)

read.csv(filename)
class(read.csv(filename))

primates <- file %>%
  filter(order == "Primates")
nrow(primates)
class(primates)

primateSleep <- primates %>%
  select(sleep_total) %>%
  unlist
class(primateSleep)
mean(primateSleep)

newMethod <- filter(file, order == "Primates") %>%
  summarize(mean(sleep_total))
newMethod
