library(tidyverse)
library(pwr)

pwr.t.test(d=1,sig.level=0.05,power=0.8,type="two.sample",alternative="two.sided")
# tells us we need 17 subjects per group to get 80% power
# in 2-tailed t-test at the 0.05 significance level
# with effect size d = 1

# simulate one experiment

control <- rnorm(17,65,5)   # your 17 simulated control data points
drug <- rnorm(17,70,5)      # your 17 simulated treatment data points

groups <- c(rep("control", 17), rep("drug", 17))   # make a vector for their names

experiment <- tibble(groups,recog=c(control,drug))
# or data.table::data.table(groups, recog = c(control, drug)) IF YOU MUST

glimpse(experiment) # our data in "long" format look right

t.test(recog ~ groups, data = experiment) # run a t-test

test_object <- t.test(recog ~ groups, data = experiment) # assign the t-test object to a thing

str(test_object) # look at the structure of the thing

test_object$p.value # you can extract the p-value from the object!

t.test(recog ~ groups, data = experiment)$p.value # or just do it directly 

# write a function that repeats what we just did

one_experiment <- function() {
  control <- rnorm(17,65,5)
  drug <- rnorm(17,70,5)
  groups <- c(rep("control", 17), rep("drug", 17))
  experiment <- tibble(groups,recog=c(control,drug))
  t.test(recog ~ groups, data = experiment)$p.value
}
# this is exactly the same sequence of commands, wrapped in a function
# the last line returns the p-value from the t-test, so that is the
# result of the function. The function only returns the result of the last line.
# Everything else is internal to the function.
# If you run this a few times, it will not change "control" or "drug" 
# from what we generated above! concept of "scoping"
# in a sense, what happens within the function stays in the function

one_experiment()
# outcome of this function is the critical test statistic for one experiment

# now you can make this process more general - introduce a parameter

one_experiment_var_n <- function(n = 17) {
  control <- rnorm(n,65,5)
  drug <- rnorm(n,70,5)
  groups <- c(rep("control", n), rep("drug", n))
  experiment <- tibble(groups,recog=c(control,drug))
  t.test(recog ~ groups, data = experiment)$p.value
}
# exactly the same, but we've replaced the 17's with the variable n.

one_experiment_var_n()
# this new function has a parameter, n, with a default value of 17.
# If we don't give it a value for the parameter, it will use 17 for n

one_experiment_var_n(n = 4)
# this gives us the p-value for one experiment with 4 per group

# do this more than once

many_experiments <- function(n = 17) {
  times <- 1000
  results <- replicate(times, one_experiment_var_n(n))
  sum(results <= 0.05) / times
}

# now we have a new function, also with a parameter n with a default value of 17
# "replicate" will repeat the second argument the number of times as the first
# so this defines a "times" value,
# does "one_experiment_var_n", with parameter n, "times" number of times
# so we get a list of p-values from 1000 experiments with n per group
# then we figure out how many of those are less than or equal to .05
# out of the number of experiments we did
# number of "significant" results out of number of experiments = power

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

# this is a simple way to do something complicated

# "map" (from the purrr package) will generate a list of the outputs of the
# second argument (a function) across the first (a list of parameters)
# map(4:25, many_experiments) would do exactly the same thing

# the map command above does the same thing as for loop
# power_levels_loop <- rep(0, 25)
# for (i in 4:25) {power_levels_loop[(i-3)] <- many_experiments(i)}
# but who wants to deal with "i-3" to put the i = 4 in the first element of your vector?
# and you have to remember to make a vector with 0's in it.
# purrr can be more flexible and has parallelized version ("furrr") that runs (even) faster

power_levels # hideous list. Who wants to deal with a list?
unlist(power_levels) # now we just have the numbers!
# not to complicate this unnecessarily, but
# map_dbl(candidate_samples, many_experiments)
# "map double" returns the result as just a vector of values
# eliminating need to unlist
# but "map" is more general. You could get a list of plots, for example,
# if the output of your function was a plot object

tibble(candidate_samples, unlist(power_levels))
# put the power values in a tibble with the Ns

tibble(candidate_samples, power=unlist(power_levels)) %>%
  ggplot(aes(x = candidate_samples, y = power)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.8, alpha = 0.6) +
  labs(x = "Sample Size per Group", y = "Power")
# shows the usefulness of the pipe construction - I can compose my little
# table of data and send it on to the ggplot command without assigning it
# to an object first
# we will see much more of ggplot, but I am telling it that I want the
# x aesthetic to be the samples - the N from 4 to 25 - and the
# y aesthetic to be the power value
# then I want it to draw points and a line
# then I can add a line to the graph where the 80% power cutoff is
# and change the axis labels
# You can customize graphs forever. You may also want to step through
# and run each bit at a time (stop highlighting before the + sign)
# and see how the plot builds up

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
# again this is exactly the same function as before
# but this new function allows me to vary the effect size as well
# convince yourself that with sd = 5 and d = 1, it's the same as before

many_experiments_var_n_d <- function(n = 17, d = 1) {
  times <- 100  # change this number if you want!
  results <- replicate(times, one_experiment_var_n_d(n,d))
  sum(results <= 0.05) / times
}
# again this is exactly like before but our many experiments version allows 2 parameters
# you can change "times" to get smoother curves
# the plot in powerpoint used times <- 1000 so yours will look different if you don't
# change the times value - it just takes longer!

parameter_space = expand_grid(n = 5:30, d = seq(0.5, 1.5, 0.1))

parameter_space # expand_grid generates all possible combinations of elements of vectors
# useful for generating power curves across a wide range of possibilities

tic()
power_levels_2 <- pmap(parameter_space, many_experiments_var_n_d)
toc()

# "pmap" is sort of a multivariate generalization of "map"
# the "many_experiments_var_n_d" function looks for 2 parameters
# so we need "pmap" instead of "map"
# there are different variations of map for specific instances
# but the general principle of streamlining iteration is the same

tibble(parameter_space, power = unlist(power_levels_2)) %>%
  mutate(d = factor(d)) %>%
  ggplot(aes(x = n, y = power)) +
  geom_point(aes(color = d)) +
  geom_line(aes(color = d, group = d)) +
  geom_hline(yintercept = 0.8, alpha = 0.6) +
  labs(x = "Sample Size per Group", y = "Power") +
  scale_color_viridis_d(end = 0.8)
# again step through this to see how it builds up 
# many addons to ggplot to individualize color etc
