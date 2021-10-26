---
title: "miceProjectR"
output: html_notebook
---
  
```{r message=FALSE, warning=FALSE}
# load libraries
library(readr)
library(dplyr)
```

```{r message=FALSE}
# load data frame
femaleMiceWeights <- read_csv("femaleMiceWeights.csv")
```

```{r}
controlGroup <- femaleMiceWeights %>%
  filter(Diet == 'hf') %>%
  select(Bodyweight)
controlGroup <- unlist(controlGroup)
```

```{r}
femaleMiceWeights <- femaleMiceWeights %>%
  select(Bodyweight)
femaleMiceWeights <- unlist(femaleMiceWeights)
control <- sample(femaleMiceWeights, 12, replace = TRUE)
mean(control)
```

```{r}
RNGkind(sample.kind = "Rounding")
set.seed(1)
ttTTF <- seq(13, 24)
sam <- sample(ttTTF, 1, replace = TRUE)
```