library(tidyverse)

n = 1000000

dataset = data.frame(
  c1 = rnorm(n),
  c2 = rnorm(n),
  c3 = rnorm(n)
) %>% mutate(
  y = (c1 + c2 + c3 + rnorm(n,0,1)) > 0
)

fit = glm(y ~ ., dataset, family = binomial)
summary(fit)

score = fit$fitted.values
response = dataset$y

roc = getROC(score,response)
lift = getLift(score,response)
lifts = lift

plotLift(lift)
