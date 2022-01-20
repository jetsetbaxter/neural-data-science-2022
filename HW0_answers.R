## R and statistics "pre test" for Neural Data Science BSR 6717 Spring 2022
## You should be able to answer these questions with just base R commands and without loading any libraries
## Googling is always encouraged.


## calculate a few probabilities - you can just enter the values as an arithmetic expression on the next line

# If you have a container with 5 red chips and 15 green chips, and you draw 1 chip, what is the probability it is red?

5/20   # 5/(5+15)

# If you have a container with 5 red chips and 15 green chips, and you draw 1 chip, what is the probability it is green?

15/20

# If you draw 3 chips from the container, what is the probability all three are green? (You return each chip to the container
# after you draw from it and shake it up before drawing the next one)

(15/20) * (15/20) * (15/20)  # sampling "with replacement"

# If you draw 3 chips from the container, what is the probability all three are green if you don't return chips to the container
# after you draw from it?

(15/20) * (14/19) * (13/18)  # sampling "without replacement"

# If you draw 3 chips from the container, what is the probability at least one is red? (You return each chip to the container
# after you draw from it and shake it up before drawing the next one)

1 - ((15/20) * (15/20) * (15/20)) # the "complement" of all 3 being green 
# P(1 red) + P(2 red) + P(3 red) also works, but you have to take into account combinatorics
# P(1 red) = P(red, green, green) + P(green, red, green) + P(green, green, red)
#          = 5/20 * 15/20 * 15/20 + 15/20 * 5/20 * 15/20 + 15/20 * 15/20 * 5/20
#          = 3 * (5/20 * 15/20 * 15/20)
# same for P(2 red) = 3 * (5/20 * 5/20 * 15/20)
# only 1 way to get 3 red (5/20 * 5/20 * 5/20)
(5/20 * 15/20 * 15/20)*3 + (5/20 * 5/20 * 15/20)*3 + (5/20 * 5/20 * 5/20)
# same result


## probability distributions: show R command(s) to calculate values

# If you have a normal distribution with mean 0 and variance 1, what fraction of the area under the curve (probability)
# is *less than* x = 0?  What fraction is *less than* x = 1?

pnorm(0)
pnorm(0, mean = 0, sd = 1, lower.tail = TRUE) # do not need to specify mean, SD, lower.tail since these are default
pnorm(1)
pnorm(1, mean = 0, sd = 1, lower.tail = TRUE)

# If you have a normal distribution with mean 0 and variance 4, what fraction of the area under the curve (probability)
# is *less than* x = 0?  What fraction is *less than* x = 1?

pnorm(0, mean = 0, sd = sqrt(4))  # if the variance is 4, the sd, which is the square root of the variance, is 2
pnorm(1, mean = 0, sd = 2)

# If you have a t-distribution with 15 degrees of freedom, what fraction of the area under the curve (probability) 
# is less than x = 1?

pt(1, df = 15)

# If you have an F-distribution with 1 and 15 degrees of freedom (numerator / denominator degrees of freedom or 
# df1 / df2 if you prefer), what fraction of the area under the curve (probability) is less than x = 1?

pf(1, df1 = 1, df2 = 15)

# What "critical value" of the F statistic corresponds to 95% of the F-distribution with 1 and 15 degrees of freedom
# being less than the critical value?

qf(0.95, df1 = 1, df2 = 15)

# Can you produce a plot that illustrates this probability graphically in R? (Just show the cutoff, don't worry about
# shading the area under the curve)

curve(df(x, df1=1, df2=15), from=0, to=6)
abline(v = qf(0.95, df1 = 1, df2 = 15), col = "red")

x = seq(0, 6, length = 500)
plot(x, df(x = x, df1 = 1, df2 = 15))
abline(v = qf(0.95, df1 = 1, df2 = 15), col = "red")


## let's run some statistical tests!

# imagine that these are test score data of 2 groups of participants, 10 in each group

group1 <- c(56, 46, 45, 42, 60, 45, 52, 59, 43, 55)
group2 <- c(85, 61, 57, 53, 64, 58, 67, 54, 76, 63)

# run a t-test comparing group1 and group2, ** assuming equal variances of the 2 groups **

t.test(group1, group2, var.equal = TRUE)
my_data <- data.frame(group = c(rep("group1",10), rep("group2", 10)), values = c(group1, group2))
t.test(values ~ group, data = my_data, var.equal = TRUE)

# What is the type 1 error associated with the t-test? (either copy from output or use an R command to display)

0.002518
t.test(group1, group2, var.equal = TRUE)$p.value  # look at output of str(t.test(group1, group2, var.equal = TRUE))
# "alpha level" of test where we would conclude "significance" is not specified (.05 typical)
# 95% confidence interval is default because of .05 significance level
# type 1 error rate is probability of observing this difference by chance when means are actually equal
# *not* probability that the means are really equal

# What is the standard deviation of group 1? what is the standard deviation of group 2?

sd(group1)
sd(group2)

# do you think the assumption of equal variances is warranted? Why or why not? How would you decide?

#### The values aren't identical but they are close to each other, especially considering the scale of the means.
#### If you run a t-test without assumption of equal variance (the default!) you get almost the same answer.
#### One may run a formal test on the equality of variances - F test (there are others, Bartlett's, Levene's)
#### Although one has to be careful making decisions based on p-values (can bias subsequent test results)
#### in fact, these data were generated from normal distributions with SD = 10 and mean = 50 (group 1) or 65 (group 2)
#### so the *population* variances are identical - but you have no way of knowing that!

var.test(group1, group2)

# The t-test provides a 95% confidence interval on the difference in means between group 1 and group 2.
# What does the 95% confidence interval mean?

#### 95% of such intervals constructed from similar samples would contain the true population mean difference.
#### - It is *not* "there is a 95% probability the true difference in means is in this interval"
#### - The true mean difference is either in that interval or not.
#### - Confidence intervals treat the population parameter as fixed and the limits as variable.
#### - Probability statements about population parameters are tricky in "frequentist" statistics. 


