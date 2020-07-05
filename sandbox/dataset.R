library(tidyverse)
library(rpart)

n = 1000000

aux_dataset = data.frame(
  c1 = rnorm(n),
  c2 = rnorm(n),
  c3 = rnorm(n)
) %>% mutate(
  y = (c1 + c2 + c3 + rnorm(n,0,1)) > 0
)

fit = glm(y ~ ., aux_dataset, family = binomial)
summary(fit)
score = fit$fitted.values

fit = rpart(y ~ ., aux_dataset)
score = predict(fit,aux_dataset)

response = aux_dataset$y

roc = getROC(score,response)
dftools::plotROC(roc)
lift = getLift(score,response)
lifts = lift

fn = makeScoreDistribution(score,response)
fn(.5)
fn(0)

plotLift(lift)
