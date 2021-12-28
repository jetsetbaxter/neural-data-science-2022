library(tidyverse)
library(pwr)

pwr.t.test(d=1,sig.level=0.05,power=0.8,type="two.sample",alternative="two.sided")

# simulate one experiment

control <- rnorm(17,65,5)
drug <- rnorm(17,70,5)

groups <- c(rep("control", 17), rep("drug", 17))

experiment <- tibble(groups,recog=c(control,drug))

glimpse(experiment)

t.test(recog ~ groups, data = experiment)

test_object <- t.test(recog ~ groups, data = experiment)

str(test_object)

test_object$p.value

t.test(recog ~ groups, data = experiment)$p.value

# write a function that repeats what we just did

one_experiment <- function() {
  control <- rnorm(17,65,5)
  drug <- rnorm(17,70,5)
  groups <- c(rep("control", 17), rep("drug", 17))
  experiment <- tibble(groups,recog=c(control,drug))
  t.test(recog ~ groups, data = experiment)$p.value
}

one_experiment()
# outcome of this function is the critical test statistic for one experiment
# now you can make this process more general

# introduce a parameter

one_experiment_var_n <- function(n = 17) {
  control <- rnorm(n,65,5)
  drug <- rnorm(n,70,5)
  groups <- c(rep("control", n), rep("drug", n))
  experiment <- tibble(groups,recog=c(control,drug))
  t.test(recog ~ groups, data = experiment)$p.value
}

one_experiment_var_n()

one_experiment_var_n(n = 4)

# do this more than once

many_experiments <- function(n = 17) {
  times <- 1000
  results <- replicate(times, one_experiment_var_n(n))
  sum(results <= 0.05) / times
}
# this adds up the number of times across 1000 experiments that p <= .05
# i.e. the power associated with a specific n
# for this particular combination of parameters (normal, mean  = 65 or 70, sd  =5)

candidate_samples <- 4:25
candidate_samples
# we will use this as a list of values for n to run many_experiments over

library(tictoc) # allows us to time the process!

set.seed(1234)

tic()
power_levels <- map(candidate_samples, many_experiments)
toc()

# "map" (purrr package) will generate a list of the outputs of the
# second argument (a function) across the first (a list of parameters)
# map(4:25, many_experiments) would work just as well
# does the same thing as for loop
# for (i in 4:25) {power_levels_loop[(i-3)] <- many_experiments(i)}
# purrr can be more flexible and has parallelized version ("furrr")

power_levels
unlist(power_levels)

tibble(candidate_samples, unlist(power_levels))
# put the power values in a tibble with the Ns

tibble(candidate_samples, power=unlist(power_levels)) %>%
  ggplot(aes(x = candidate_samples, y = power)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.8, alpha = 0.6) +
  labs(x = "Sample Size per Group", y = "Power")

# you can generalize this to different mean differences easily enough

set.seed(1234)

one_experiment_var_n_d <- function(n = 17, d = 1) {
  sd <- 5
  control <- rnorm(n,65,sd)
  drug <- rnorm(n,65+(d*sd),sd)
  groups <- c(rep("control", n), rep("drug", n))
  experiment <- tibble(groups,recog=c(control,drug))
  t.test(recog ~ groups, data = experiment)$p.value
}

many_experiments_var_n_d <- function(n = 17, d = 1) {
  times <- 100
  results <- replicate(times, one_experiment_var_n_d(n,d))
  sum(results <= 0.05) / times
}
# can increase "times" to get smoother curves
# plot in powerpoint used times <- 1000

parameter_space = expand_grid(n = 5:30, d = seq(0.5, 1.5, 0.1))

parameter_space # expand_grid generates all possible combinations of elements of vectors
# useful for generating power curves across a wide range of possibilities

tic()
power_levels_2 <- pmap(parameter_space, many_experiments_var_n_d)
toc()

tibble(parameter_space, power = unlist(power_levels_2)) %>%
  mutate(d = factor(d)) %>%
  ggplot(aes(x = n, y = power)) +
  geom_point(aes(color = d)) +
  geom_line(aes(color = d, group = d)) +
  geom_hline(yintercept = 0.8, alpha = 0.6) +
  labs(x = "Sample Size per Group", y = "Power") +
  scale_color_viridis_d(end = 0.8)
