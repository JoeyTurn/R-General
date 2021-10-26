d = read.csv("assoctest.csv")
table(d)
chisq.test(table(d))
fisher.test(table(d))

load("skew.RData")
dim(dat)
par(mfrow = c(3,3))
for (i in 1:9) {
  qqnorm(dat[,i], main=i)
}
