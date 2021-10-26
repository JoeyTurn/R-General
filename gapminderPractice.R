install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

x <- gapminder %>%
  filter(year == 1952) %>%
  select(lifeExp) %>%
  unlist

mean(x <= 40)
mean(x <= 60)-mean(x <= 40)

prop = function(q) {
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
print(qs)
#could have also been written like this
#props = sapply(qs, function(q) mean(x <= q))

props = sapply(qs, prop)
plot(props)

plot(ecdf(x))
