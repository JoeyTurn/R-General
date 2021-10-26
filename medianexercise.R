head(InsectSprays)
boxplot(split(InsectSprays$count, InsectSprays$spray))

library(dplyr)
data(nym.2002, package="UsingR")
str(nym.2002)
males<-filter(nym.2002, gender=="Male") 
females<-filter(nym.2002, gender=="Female") 
boxplot(males$time, females$time)
hist(males$time)
hist(females$time)
groups <- split(females$time, round(females$age))
boxplot(groups, females$time)
groupsm <- split(males$time, round(males$age))
boxplot(groupsm, males$time)

time = sort(nym.2002$time)
max(time)/median(time)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet, main = "Chicken Time vs Weight", xlab = "Time", ylab = "Weight")
plot(chick$weight.4, chick$weight.21)

cor(chick$weight.4, chick$weight.21)

chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")

head(chick)
chick = na.omit(chick)
chicknew <-c(chick$weight.4, 3000)
chicknew2 <- c(chick$weight.21, 3000)
plot(chicknew, chicknew2)
mad(chicknew)/mad(chick$weight.4)
