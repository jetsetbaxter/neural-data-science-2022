## R and statistics "pre test" for Neural Data Science BSR 6717 Spring 2022
## You should be able to answer these questions with just base R commands and without loading any libraries
## Googling is always encouraged.


## calculate a few probabilities - you can just enter the values as an arithmetic expression on the next line

# If you have a container with 5 red chips and 15 green chips, and you draw 1 chip, what is the probability it is red?

# If you have a container with 5 red chips and 15 green chips, and you draw 1 chip, what is the probability it is green?

# If you draw 3 chips from the container, what is the probability all three are green? (You return each chip to the container
# after you draw from it and shake it up before drawing the next one)

# If you draw 3 chips from the container, what is the probability all three are green if you don't return chips to the container
# after you draw from it?

# If you draw 3 chips from the container, what is the probability at least one is red? (You return each chip to the container
# after you draw from it and shake it up before drawing the next one)


## probability distributions: show R command(s) to calculate values

# If you have a normal distribution with mean 0 and variance 1, what fraction of the area under the curve (probability)
# is *less than* x = 0?  What fraction is *less than* x = 1?

# If you have a normal distribution with mean 0 and variance 4, what fraction of the area under the curve (probability)
# is *less than* x = 0?  What fraction is *less than* x = 1?

# If you have a t-distribution with 15 degrees of freedom, what fraction of the area under the curve (probability) 
# is less than x = 1?

# If you have an F-distribution with 1 and 15 degrees of freedom (numerator / denominator degrees of freedom or 
# df1 / df2 if you prefer), what fraction of the area under the curve (probability) is less than x = 1?

# What "critical value" of the F statistic corresponds to 95% of the F-distribution with 1 and 15 degrees of freedom
# being less than the critical value?

# Can you produce a plot that illustrates this probability graphically in R?


## let's run some statistical tests!

# imagine that these are test score data of 2 groups of participants, 10 in each group

group1 <- c(56, 46, 45, 42, 60, 45, 52, 59, 43, 55)
group2 <- c(85, 61, 57, 53, 64, 58, 67, 54, 76, 63)

# run a t-test comparing group1 and group2, ** assuming equal variances of the 2 groups **

# What is the type 1 error associated with the t-test? (either copy from output or use an R command to display)

# What is the standard deviation of group 1? what is the standard deviation of group 2?

# do you think the assumption of equal variances is warranted? Why or why not? How would you decide?

# The t-test provides a 95% confidence interval on the difference in means between group 1 and group 2.
# What does the 95% confidence interval mean?



