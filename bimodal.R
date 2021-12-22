set.seed(1234)
# this sets a seed value for the random number generation
# so if you run this exact sequence of commands more than once you get the same result
# try running this more than once and then change the seed value!

bimodal <- c(rnorm(100,10,2),rnorm(100,20,2))
# can type ?rnorm at prompt
# rnorm takes 3 values: number of "draws", mean, standard deviation
# this generates 100 draws from a normal with mean 10 and SD 2
# and 100 draws from a normal with mean 20 and SD 2
# and then sticks them together into a vector

hist(bimodal, breaks = 20) # plot the data we just generated

## The central limit theorem works a couple of ways.

# We can generate many "bimodal"s and look at the mean of each one

mean(bimodal) # about 15 
# halfway between the means of the two normal samples we stuck together
# makes sense

another_bimodal <- c(rnorm(100,10,2),rnorm(100,20,2))

mean(another_bimodal) # not identical but also about 15

many_means <- replicate(10000, mean(c(rnorm(100,10,2),rnorm(100,20,2))))

hist(many_means) # symmetric and centered on 15

sum(((many_means > 14.8) & (many_means < 15.2)))/10000
# ~ 85% between 14.8 and 15.2
# R trick: TRUE is 1 and FALSE is 0
# get all the many_means that are greater than 14.8
# get all the many_means that are less than 15.2
# combine these with "and" - TRUE and TRUE is TRUE but TRUE and FALSE is FALSE
# now you have a vector of 10000 TRUE or FALSE where TRUE indicates that value
# is between 14.8 and 15.2
# add up the vector: how many trues?
# divide by the number of values: percentage of means in that interval

## What if, instead of doing 10000 experiments, we draw samples from our bimodal

bimodal

sample(bimodal, 10, replace = TRUE) # picks 10 values from the 200 in bimodal

mean(sample(bimodal, 10, replace = TRUE)) # oh-ho, the mean is also about 15

bimodal_means <- replicate(10000, mean(sample(bimodal, 10, replace = TRUE)))
# do this 10000 times

hist(bimodal_means) # normal, centered on 15, but "wider"
sum(((bimodal_means > 14.8) & (bimodal_means < 15.2)))/10000
# only ~9% in that same interval between 14.8 and 15.2
# intuitively, mean of small sample "noisier" - higher SD / variance

## We stuck 2 normal distributions together. But this works even 
## if the random variables are not normal at all
## Also, of course, sticking 2 normals together produces a non-normal
## distribution!

bimodal2 <- c(runif(100, 8, 12), runif(100,18,22))
hist(bimodal2, breaks = 20) # 2 uniforms: 0 values between 12 and 18!
mean(bimodal2) # but the mean of these is a value that does not exist in the data!

bimodal_means2 <- replicate(10000, mean(sample(bimodal2, 10, replace = TRUE)))
# draw many samples from that bimodal/uniform distribution

hist(bimodal_means2)
# again means are normally distributed and centered on mean of underlying data