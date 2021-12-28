library(tidyverse)
library(pwr)        # install.packages("pwr")
library(gganimate)  # install.packages("gganimate")

# Does it annoy you that you don't know when to quote and when not to?
# If something is already in the R environment, it doesn't need quotes
# If it isn't, it does
# So the library name goes in quotes when you install it but not when you load it
# This applies with tidyverse functions too, in general


set.seed(1234) # set a random number seed so these results can be reproduced

control <- rnorm(200, mean = 100, sd = 15)
manip <- rnorm(200, mean = 110, sd = 15)
# create two vectors of random normal variates
# one with mean 100, one with mean 110, both with SD 15
# 200 in each vector

data10 <- tibble(measure = c(control, manip), 
                 group = c(rep("control", 200), rep("manip", 200)), 
                 sample = rep(1:200, 2))
# this creates a data frame ("tibble") in "long" format
# the control and manip numbers are stacked up
# there is a "sample" column that runs 1 to 200 for each group

pwr.t.test(d = 0.67, power = 0.8, sig.level=0.05, type="two.sample", alternative="two.sided")
# calculate power of t-test with d = 0.67
# when samples == 36 we should expect p to be below .05 80% of the time

march_p <- function(samples = 15) {
  tempdata <- data10 %>% filter(sample <= samples)
  t.test(measure ~ group, data = tempdata)$p.value
}
# break down the steps in this function
# set a parameter "samples" with a default value of 15
# create a data frame, tempdata, containing the elements of data10
# where sample is less than or equal to "samples"
# so if "samples" is 15, you are left with the first 15 control and the first 15 manip
# then run a t-test on this sub-sample of data and just return the p-value
march_p()

pvalues <- map(2:200, march_p) %>% unlist()
# "map" will iterate the second argument across the first
# this produces a list of march_p(samples = 2), march_p(samples = 3), ...
# "unlist" converts from list format to just a vector of numbers

tibble(n = 2:200, pvalues) %>%
  ggplot(aes(x=n,y=pvalues)) +
  geom_line() +
  geom_hline(yintercept = 0.05,alpha=0.4)
# we can plot how the p-values change as we add to the sample from n = 2 to 200
# looks nice and orderly right?

# generate another sample

set.seed(2468)

control <- rnorm(200, mean = 100, sd = 15)
manip <- rnorm(200, mean = 110, sd = 15)

data10 <- tibble(measure = c(control, manip), 
                 group = c(rep("control", 200), rep("manip", 200)), 
                 sample = rep(1:200, 2))

march_p <- function(samples = 15) {
  tempdata <- data10 %>% filter(sample <= samples)
  t.test(measure ~ group, data = tempdata)$p.value
}

pvalues <- map(2:200, march_p) %>% unlist()

tibble(n = 2:200, pvalues) %>%
  ggplot(aes(x=n,y=pvalues)) +
  geom_line() +
  geom_hline(yintercept = 0.05,alpha=0.4)
# still pretty orderly. Who needs n = 36?

# generate a third sample

set.seed(3579)

control <- rnorm(200, mean = 100, sd = 15)
manip <- rnorm(200, mean = 110, sd = 15)

data10 <- tibble(measure = c(control, manip), 
                 group = c(rep("control", 200), rep("manip", 200)), 
                 sample = rep(1:200, 2))

march_p <- function(samples = 15) {
  tempdata <- data10 %>% filter(sample <= samples)
  t.test(measure ~ group, data = tempdata)$p.value
}

pvalues <- map(2:200, march_p) %>% unlist()

tibble(n = 2:200, pvalues) %>%
  ggplot(aes(x=n,y=pvalues)) +
  geom_line() +
  geom_hline(yintercept = 0.05,alpha=0.4)
# perhaps we would really like n = 36 in each group!
# this starts to show the issues with collecting data sequentially and testing as you go
# the p-values can go up and down as data are accumulated
# but if there's an effect in the population, they should go below .05 and stay there

pvalues <= 0.05 # when you hit 33 in each sample, p goes below .05 and stays there

tibble(n = 2:200, pvalues) %>%
  ggplot(aes(x=n,y=pvalues)) +
  geom_line() +
  geom_hline(yintercept = 0.05,alpha=0.4) +
  transition_reveal(n) # produces animated gif (gganimate package)

anim_save("nicemarch.gif") # gganimate package allows saving animated gifs!

# generate a fourth sample

# let's wrap the whole thing in a function 

generate_march <- function(seed, control_mean = 100, manip_mean = 110) {
  set.seed(seed)
  control <- rnorm(200, mean = control_mean, sd = 15)
  manip <- rnorm(200, mean = manip_mean, sd = 15)
  data_gen <- tibble(measure = c(control, manip), 
                   group = c(rep("control", 200), rep("manip", 200)), 
                   sample = rep(1:200, 2))
  march_p_gen <- function(samples = 15) {
    tempdata <- data_gen %>% filter(sample <= samples)
    t.test(measure ~ group, data = tempdata)$p.value }
  pvalues <- map(2:200, march_p_gen) %>% unlist()
  tibble(n = 2:200, pvalues) %>%
    ggplot(aes(x=n,y=pvalues)) +
    geom_line() +
    geom_hline(yintercept = 0.05,alpha=0.4)
}
# changed "data10" and "march_p" so they are only defined within function
# odd things can happen when you call things within functions that are
# part of the global environment ("scoping")
# R will look within the function first and then look outside
# can also use "anonymous function" with map (google it) instead of defining march_p_gen
# "seed" does not have a default so we must specify it each time
# if we don't specify control_mean or manip_mean, they stay at 100 and 110
# we could make this more general by adding an SD parameter, or even two SD parameters

generate_march(1313)

# what if we do this exercise when there is no effect?

generate_march(seed = 2345, manip_mean = 100)

# generate another sample

generate_march(4444, manip_mean = 100) # when parameters in order, don't need to name
# but if I didn't name manip_mean, it would think I meant control_mean = 100
# but generate_march(4444, 100, 100) would work

generate_march_gif <- function(seed, control_mean = 100, manip_mean = 110) {
  set.seed(seed)
  control <- rnorm(200, mean = control_mean, sd = 15)
  manip <- rnorm(200, mean = manip_mean, sd = 15)
  data_gen <- tibble(measure = c(control, manip), 
                     group = c(rep("control", 200), rep("manip", 200)), 
                     sample = rep(1:200, 2))
  march_p_gen <- function(samples = 15) {
    tempdata <- data_gen %>% filter(sample <= samples)
    t.test(measure ~ group, data = tempdata)$p.value }
  pvalues <- map(2:200, march_p_gen) %>% unlist()
  tibble(n = 2:200, pvalues) %>%
    ggplot(aes(x=n,y=pvalues)) +
    geom_line() +
    geom_hline(yintercept = 0.05,alpha=0.4) +
    transition_reveal(n)
}

generate_march_gif(4444, manip_mean = 100)
anim_save("null1.gif")

# one more time

generate_march(3333, manip_mean = 100)

generate_march_gif(3333, manip_mean = 100)
anim_save("null2yikes.gif")
  
# yikes!
# even when no effect in population, possible to make a type 1 error
# if you actually did this experiment, you might convince yourself at N ~ 30 you were close


