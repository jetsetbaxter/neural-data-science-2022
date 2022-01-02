# R studio: 
# command-return executes current line of code
# or next line of code if you're on a comment line
# or you can highlight a block of code and execute it
# useful for stepping through a script

group1 <- c(56, 46, 45, 42, 60, 45, 52, 59, 43, 55)
group2 <- c(85, 61, 57, 53, 64, 58, 67, 54, 76, 63)

t.test(group1, group2, var.equal = TRUE)
# assumes underlying normality of population from which data are sampled
# assumes variance in group1 and group2 equal
# (a correction factor is available if the second assumption may not hold)

## permutation testing

# permutation approach uses the data to generate a distribution from which we can test
# no assumptions about population distribution or anything else

# we need a point statistic to estimate from each permutation
# them we see how many times we get values of that point statistic that equal or exceed 
# the value in our actual data

# the difference in mean between group 2 and group 1 would do nicely

mean(group1)
mean(group2)
meandiff <- mean(group2) - mean(group1)
meandiff

# how do we generate a permutation?
# sample function in R will do it for us nicely
# let's make one vector with all the data

group_data <- c(group1, group2)
group_data
# just group1 and group 2 stuck together

# if we take the mean of group2 (elements 11 to 20) and subtract the mean of group1 (elements 1 to 10)
# we get the same answer as before

mean(group_data[11:20])
mean(group_data[1:10])
mean(group_data[11:20])-mean(group_data[1:10])

# if we jumble up the numbers in the vector and subtract the mean of the first ten from the
# mean of the second test, we are generating the same statistic on permuted data
# to jumble up the data, we use the sample function

sample(group_data, 20)
# the second number is the sample size (in this case 20)
# we could use length(group_data) to make more general
# there is a replace parameter which is FALSE by default
# replace = TRUE will sample with replacement
# we might want to do that sometimes, but not in this case where we just want to permute

sample(group_data, 20)
# if we do this again, we get a different sample

group_sample <- sample(group_data, 20)
group_sample
mean(group_sample[11:20])
mean(group_sample[1:10])
mean(group_sample[11:20])-mean(group_sample[1:10])

group_sample <- sample(group_data, 20)
mean(group_sample[11:20])
mean(group_sample[1:10])
mean(group_sample[11:20])-mean(group_sample[1:10])

# 1000 times ought to do it
# let's wrap this in a for loop so we don't get a repetitive strain injury

# first create a vector to save the results

group_sample_mean_diffs <- rep(0, 1000)
group_sample_mean_diffs  # a thousand zeroes. Groundbreaking

for (i in 1:1000) {
  group_loop_sample <- sample(group_data, 20)
  group_sample_mean_diffs[i] <- mean(group_loop_sample[11:20])-mean(group_loop_sample[1:10])
}
# I am superstitious / skittish about reusing variable names in a loop I've used outside

group_sample_mean_diffs

mean(group_sample_mean_diffs)
# the mean is about 0

plot(group_sample_mean_diffs)
# that's not super informative

hist(group_sample_mean_diffs)
# that's nore useful

meandiff

# our actual value our real data seems pretty unlikely

group_sample_mean_diffs >= meandiff
# okay that is not super useful
# but remember FALSE gets treated as a 0 and TRUE as 1

sum(group_sample_mean_diffs >= meandiff)
# wrap the above expression in a sum() function
# that's how many times you get a difference as extreme or nore extreme than in our real data

sum(group_sample_mean_diffs >= meandiff)/1000
# look, a p-value!
# could divide by length(group_sample_mean_diffs) to make more general
# for example, if you were varying the number of simulations
mean(group_sample_mean_diffs >= meandiff) # also works

# what if we wanted a "two-tailed" p-value?
# absolute values of mean differences

abs(group_sample_mean_diffs)

sum(abs(group_sample_mean_diffs) >= meandiff)/1000

# does everyone get the same answer?

set.seed(20220124) # sets seed for random number generator

group_sample_mean_diffs <- rep(0, 1000) # set the vector back to 0

for (i in 1:1000) {
  group_loop_sample <- sample(group_data, 20)
  group_sample_mean_diffs[i] <- mean(group_loop_sample[11:20])-mean(group_loop_sample[1:10])
}

sum(abs(group_sample_mean_diffs) >= meandiff)/1000
# everyone, hopefully, gets 0.001
# important to set a random number seed if you're using simulations in your research


set.seed(012422)

group_sample_mean_diffs <- rep(0, 1000) # set the vector back to 0

for (i in 1:1000) {
  group_loop_sample <- sample(group_data, 20)
  group_sample_mean_diffs[i] <- mean(group_loop_sample[11:20])-mean(group_loop_sample[1:10])
}

sum(abs(group_sample_mean_diffs) >= meandiff)/1000
# different seed ... different answer
