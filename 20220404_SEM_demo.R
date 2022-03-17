library(tidyverse)
library(lavaan)

data("HolzingerSwineford1939")
?HolzingerSwineford1939

hs1939 <- HolzingerSwineford1939 # we are not typing this out every time
glimpse(hs1939)

# x1 = Visual perception
# x2 = Cubes
# x3 = Lozenges
# x4 = Paragraph comprehension
# x5 = Sentence completion
# x6 = Word meaning
# x7 = Speeded addition
# x8 = Speeded counting of dots
# x9 = Speeded discrimination straight and curved capitals

## start small

## specify the model
x1to6_model <- ' visual =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6'
# =~ "is measured by"
# latent variable analog of y ~ x1 + x2 + x3 in linear model

## confirmatory factor analysis: fit the model
fit1 <- cfa(model = x1to6_model, data = hs1939)

## display summary output
summary(fit1, fit.measures = TRUE)

## install.packages("effectsize")
effectsize::interpret(fit1) # based on general guidelines (dark magic)

## install.packages("lavaanPlot")
library(lavaanPlot)
lavaanPlot(model = fit1, coefs = TRUE)

## 3 factor CFA

## specify the model
x1to9_model <- ' visual =~ x1 + x2 + x3
                textual =~ x4 + x5 + x6
                  speed =~ x7 + x8 + x9'
# =~ "is measured by"
# latent variable analog of y ~ x1 + x2 + x3 in linear model

## confirmatory factor analysis: fit the model
fit2 <- cfa(model = x1to9_model, data = hs1939)

## display summary output
summary(fit2, fit.measures = TRUE)
effectsize::interpret(fit2)

## how could we improve fit?
modindices(fit2, sort = TRUE, maximum.number = 8)

x1to9_model_mod <- ' visual =~ x1 + x2 + x3 + x7 + x9
                    textual =~ x4 + x5 + x6
                      speed =~ x7 + x8 + x9'

fit3 <- cfa(model = x1to9_model_mod, data = hs1939)

summary(fit3, fit.measures = TRUE)
effectsize::interpret(fit3)
# fit indices improve
# but we have blurred the line between confirmatory and exploratory models

## maybe we don't need the speed factor? do the speed variables just go with visual?

x1to9_2factor <- 'visspeed =~ x1 + x2 + x3 + x7 + x8 + x9
                   textual =~ x4 + x5 + x6'

fit4 <- cfa(model = x1to9_2factor, data = hs1939)

summary(fit4, fit.measures = TRUE)
effectsize::interpret(fit4)
# oh no that's much worse
# you can see how you can go down the rabbit hole very quickly

## full latent variable model (modified from lavaan tutorial)

data("PoliticalDemocracy")
?PoliticalDemocracy
poldem <- PoliticalDemocracy
glimpse(poldem)
# x1-x3 are economic indicators in 1960
# y1-y4 are indicators of democracy in 1960
# y5-y8 are the same as y1-y4 in 1965

pdmodel1 <- '
# measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
# regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60'

## note now we have a measurement model where =~ relates indicators to latent variables
## we also have a structural (regression) model that relates latent variables to one another
## democracy in 1960 is influenced by economic indicators in 1960
## democracy in 1965 is influenced by economic indicators in 1960 AND democracy in 1960

pd_fit1 <- sem(model = pdmodel1, data = poldem)
summary(pd_fit1, standardized = TRUE, fit.measures = TRUE)
lavaanPlot(model = pd_fit1, coefs = TRUE)

## y1-y4 and y5-y8 are the same variables measured in 1960 and 1965
## reasonably, the measurement error in the same instruments may be correlated
## the ~~ operator allows specification of correlation/covariance between indicators

pdmodel2 <- '
# measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
# regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
# residual correlations
    y1 ~~ y5
    y2 ~~ y6
    y3 ~~ y7
    y4 ~~ y8'

pd_fit2 <- sem(model = pdmodel2, data = poldem)
summary(pd_fit2, standardized = TRUE, fit.measures = TRUE)
lavaanPlot(model = pd_fit2, coefs = TRUE)
## fit improves
## example on tutorial site includes y2 ~~ y4 and y6 ~~ y8 as well (not sure why)

