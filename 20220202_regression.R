library(tidyverse)

set.seed(1111)

x <- rnorm(50,0,1)
y <- rnorm(50,0,0.2) + x
data1 <- tibble(x,y)

model1 <- lm(y ~ x, data = data1)
summary(model1)

data1 %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm")

plot(fitted(model1), resid(model1)) # functions operate on the model object to provide fitted values and residuals
qqnorm(rstandard(model1)) # Q-Q plot, slightly less annoying than pressing return to page through plot() output

y_wiggle <- c(runif(20,-3,3), rep(0, 30)) # add uniformly distributed noise to first 20 data points in the set of 50

data1$y2 <- data1$y + y_wiggle

model2 <- lm(y2 ~ x, data = data1)
summary(model2)

data1 %>% ggplot(aes(x=x, y=y2)) + geom_point() + geom_smooth(method = "lm")

plot(fitted(model2), resid(model2)) 
qqnorm(rstandard(model2))

