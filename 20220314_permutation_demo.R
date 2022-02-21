library(tidyverse)

set.seed(333)

numtrials <- 500
varrange <- seq(18,32,1) # sequence of integers to select variance

trials <- rep(0, numtrials)   # preallocate vector for trial-by-trial responses

firingrates1 <- seq(2, 5, 0.1)   # possible mean firing rates for each condition
firingrates2 <- seq(5, 8, 0.1)
firingrates3 <- seq(8, 11, 0.1)
firingrates4 <- seq(11, 14, 0.1)

firingrates <- list(firingrates1, firingrates2, firingrates3, firingrates4)

# randomly assign each trial to a condition

condition <- sample(1:4, 500, replace = TRUE)

meanresponse <- vector(mode = "list", length = 4) # initialize empty list

# select a mean firing rate for each condition
for (k in 1:4) {
  meanresponse[[k]] <- sample(firingrates[[k]], 1)
}

meanresponse

var <- sample(varrange, 1) # sample from the range of possible variances
for (k in 1:numtrials) {
  noise = rnorm(1, 0, var) # noise is random normal with SD 0 and standard deviation var
  trials[k] <- meanresponse[[condition[k]]] + noise
}

data.frame(condition, trials) %>% mutate(condition = factor(condition)) %>%
  ggplot(aes(x = condition, y = trials)) +
  stat_summary(fun = "mean", geom = "col") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2)

lm(trials ~ condition) %>% summary() # assumes linear increase in firing rate with trial
cor(trials, condition)
cor.test(condition,trials)

# construct permutation test

numshuf <- 10000
Rs <- rep(0, numtrials)   # empty vector for correlations of shuffled data

for (shuf in 1:numshuf) {
  Rs[shuf] = cor(sample(condition, length(condition), replace = FALSE), trials)
}

hist(Rs, breaks = 50)
abline(v = cor(trials, condition), col = "red")

# get p-value

mean(abs(Rs) >= cor(trials, condition))
# take absolute value of correlations - two tailed test
# logical statement vector >= constant generates vector of 1s and 0s 
# where 1 means that value in original vector met the condition
# i.e. greater than or equal to cor
# and is 0 otherwise
# the mean will just tell you what fraction of the values are 1
# in this case, what fraction of Rs from shuffled data greater than or equal to your real R
# which is the p-value for the permutation test!
# test it out: 

c(1, 2, 3, 4, 5, 6, 7, 1, 9, 4, 6, 9, 15, 3) >= 7

mean(c(1, 2, 3, 4, 5, 6, 7, 1, 9, 4, 6, 9, 15, 3) >= 7)
