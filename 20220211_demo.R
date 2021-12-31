library(tidyverse)
library(lme4)
library(lmerTest)

# additive ANOVA model with one factor

A <- c(62,60,63,59)
B <- c(63,67,71,64,65,66)
C <- c(68,66,71,67,68,68)
D <- c(56,62,60,61,63,64,63,59)

data1 <- tibble(group = c(rep("A", length(A)),
                          rep("B", length(B)),
                          rep("C", length(C)),
                          rep("D", length(D))),
                obj_time = c(A,B,C,D))

glimpse(data1)

data1 %>% summarize(mean(obj_time)) # mean(data1$obj_time)

mean(A) # data1 %>% filter(group == "A") %>% summarize(mean(obj_time))
mean(B)
mean(C)
mean(D)

data1 %>% group_by(group) %>% summarize(mean(obj_time))

data1 <- data1 %>% mutate(grand_mean = mean(obj_time))  # grand mean is mean of all data points

data1 <- data1 %>% group_by(group) %>% mutate(group_mean = mean(obj_time)) %>% ungroup()

data1

data1 <- data1 %>% mutate(group_effect = group_mean - grand_mean,
                          error = obj_time - group_mean)
# group effect is difference between group mean and grand mean
# error is difference between data point and group mean
# best prediction if you don't know group is grand mean
# best prediction if you do know group is group mean

data1 %>% print(n=Inf)

data1$grand_mean^2

# sums of squares ... add together
sum(data1$obj_time^2)
sum(data1$grand_mean^2)
sum(data1$group_effect^2)
sum(data1$error^2)

sum(data1$obj_time^2) == sum(data1$grand_mean^2) + sum(data1$group_effect^2) + sum(data1$error^2)
# evaluates whether sum of squared data points equals sums of squares of components, and it does

# remember sample variance formula: sum of deviations from mean squared over n - 1
# that "n - 1" deducts a "degree of freedom" because you have estimated the sample mean to get the
# deviations from the mean

# degrees of freedom: if you estimate a mean or other quantity you lose one
# if I know the mean of a sample of 3 data points is 10
# and I also know that two of the three data points are 8 and 9
# then I can calculate that the third data point has to be 13 because (8+9+?)/3 has to equal 10
# I only have two "degrees of freedom" for the data points once I know the overall mean

sum(data1$group_effect^2)/3
sum(data1$error^2)/20

# dividing a sum of squared differences by the degrees of freedom gives an estimate of the variance
# both of these are estimates of variance in data set if group is uninformative about value of data points
# but if the group contains information about the value of the data points then
# the estimate of variance based on group mean deviations will be inflated
# we can test whether the ratio of these estimates differs from 1
# the ratio of two sample variances is distributed as an F distribution

(f_stat_1 <- (sum(data1$group_effect^2)/3)/(sum(data1$error^2)/20))

x <- rf(100000, df1 = 3, df2 = 20)
hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,10), 
     ylim = c(0,1), main = "F-distribution with 3,20 df",
     xlab = '')

curve(df(x, df1 = 3, df2 = 20), from = 0, to = 11, n = 5000, col= 'pink', lwd=2, add = T)

# can see that this F statistic is extreme - wayyyy out in the tail 
# very different from expected value around 1

## back to ANOVA!

glimpse(data1)

lm(obj_time ~ group, data = data1) %>% anova()
# these numbers should all be familiar

aov(obj_time ~ group, data = data1) %>% summary()
# "aov" analysis of variance function is just a wrapper on lm

f_stat_1

pf(f_stat_1, df1 = 3, df2 = 20, lower.tail = FALSE) # same value as lm / ANOVA

## what about regression?

# make dummy-coded variables
data1 <- data1 %>% mutate(B_group = case_when(group == "B" ~ 1,
                                              TRUE ~ 0),
                          C_group = case_when(group == "C" ~ 1,
                                              TRUE ~ 0),
                          D_group = case_when(group == "D" ~ 1,
                                              TRUE ~ 0))

data1 %>% print(n = Inf)
# we need 3 dummy coded variables to summarize 4 groups
# if a data point is not in B, C, or D, it must be A
# thus A is the "reference level"
# 3 dummy coded variables <-> 3 degrees of freedom in group effect

lm(obj_time ~ B_group + C_group + D_group, data = data1) %>% summary()
# Intercept is mean of group A ... "reference level"
# coefficient of group B is the difference between mean of A and mean of B
# coefficient of group C is the difference between mean of A and mean of C
# coefficient of group D is the difference between mean of A and mean of D

lm(obj_time ~ group, data = data1) %>% summary()
# let R do it by itself and produces the same coefficients
# notice: the F statistic for the multiple R squared is the same as we calculated for ANOVA
lm(obj_time ~ group, data = data1) %>% anova()

