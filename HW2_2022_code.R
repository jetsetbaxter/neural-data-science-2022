library(tidyverse)
library(readxl)

hw1_data <- read_excel("HW1_2022_data.xlsx", skip = 1) %>%
  filter(!is.na(Genotype)) %>% filter(!str_detect(Genotype, "mean")) %>%
  mutate(Genotype = case_when(str_detect(Genotype, "[wW]") ~ "wild_type",
                              str_detect(Genotype, "[kK]") ~ "knockout")) %>%
  mutate(Genotype = factor(Genotype, levels = c("wild_type", "knockout"))) %>%
  pivot_longer(`Trial 1`:`Trial 3`, names_to = "Trial", 
               values_to = "Investigation_Time") %>%
  mutate(Trial = factor(parse_number(Trial)))

##
## Fundamentally this is straightforward.
## Figure out the effect size for wild-type vs knockout (Cohen's d)
## And plug it into the pwr.t.test() function from the pwr library
## But what's the right way to get the effect size? 
##
## One option is just to grab an off-the-shelf function and let it worry about it

hw1_data %>% rstatix::cohens_d(Investigation_Time ~ Genotype) # can make this very simple
pwr::pwr.t.test(d = 1.44, sig.level = 0.05, power = 0.8) # defaults two-sample, two-tailed

# we could stop there! you need N = 9 per group with 80% power at .05 level
# but let's explore the process a little

# cheeky graph just to see what the data are like

# all of the individual trials separately
# if you think of the 3 trials as independent, it's like you just ran 
# separate mice on each trial, so like you have 21 WT data points and 27 KO data points

hw1_data %>%
  ggplot(aes(x = Genotype, y = Investigation_Time, color = Genotype)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean,
               geom = "errorbar", width = 0.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x)) +
  ylim(c(90,170))

# stat_summary will plot functions of data, predefined or user-specified
# function(x) mean(x) + sd(x) is an example of an "anonymous function"
# could have also defined error_max <- function(x) {mean(x) + sd(x)} outside ggplot
# and then set fun.max = error_max
# did I google getting standard deviation error bars? absolutely

# consider the mean of each mouse across all 3 trials for that mouse
# this is a bit different - assumes that you can zero in on each mouse's score by averaging

hw1_data %>%
  group_by(Mouse, Genotype) %>%
  summarize(mean_investigation_time = mean(Investigation_Time)) %>%
  ungroup() %>%
  ggplot(aes(x = Genotype, y = mean_investigation_time, color = Genotype)) +
  geom_jitter(width = 0.1) +
  stat_summary(fun = mean,
               geom = "errorbar", width = 0.2,
               fun.max = function(x) mean(x) + sd(x),
               fun.min = function(x) mean(x) - sd(x)) +
  ylim(c(90,170))

# notice that the error bars are smaller - SD estimates are smaller
# this is a side effect of averaging
# if you were going to run each mouse 3 times in your planned study
# then this would be a good way to go - but you aren't!

## for power analysis
           
library(pwr)

# how to get estimate of SD for power calculation? Cohen's d is mean difference divided by SD

# let's break it down and calculate Cohen's d by hand

# where does SD come from, and why do we need pooled SD?

hw1_data %>% 
  summarize(SD_Investigation_Time = sd(Investigation_Time))

# SD from entire sample is 18. Appropriate? ** no **
# Does not take into account difference in groups. 
# wild type and KO are not drawn from same population
# so it does not make sense to combine them to estimate population SD

(all_trials_summary <- hw1_data %>% group_by(Genotype) %>%
    summarize(Mean = mean(Investigation_Time),
            SD = sd(Investigation_Time)))
# enclose whole thing in parentheses - displays outcome AND assigns to an object
# useful trick!
# anyway ... this calculation treats all trials as data, so unit of comparison is single trial
# SD is 10.3 for WT and 17.6 for knockout
# consistent with graph we made

# what if we average each mouse's data across the 3 trials?

(meanof3_summary <- hw1_data %>% group_by(Mouse, Genotype) %>% 
    summarize(mean_time_per_mouse = mean(Investigation_Time)) %>%
  ungroup() %>%
  group_by(Genotype) %>%
    summarize(Mean = mean(mean_time_per_mouse),
            SD = sd(mean_time_per_mouse)))
# means are the same but SDs are smaller (4.23 and 10.3) - again consistent with graphing
# mean per mouse - smaller estimates of SD - averaging effect/central limit theorem
# compare the two graphs above and how the spread of points differs
# if you were going to run each mouse 3 times in your planned study this is fine
# but that is not what you are planning to do!

# pooled SD: square root of mean of squared SDs ((SD1^2 + SD2^2)/2)^0.5
# you can't just average SDs to get pooled SD because really you should be averaging variances
# and square root is noncommutative - sqrt(a + b) != sqrt(a) + sqrt(b)

all_trials_summary
all_trials_summary$SD
all_trials_summary$SD[1]
all_trials_summary$SD[2] # can grab SDs directly from summary table

pooledsd1 <- ((all_trials_summary$SD[1]^2 + all_trials_summary$SD[2]^2)/2)^0.5 # each trial independent
pooledsd2 <- ((meanof3_summary$SD[1]^2 + meanof3_summary$SD[2]^2)/2)^0.5 # mean of 3 trials

# or, if you like 

pooled_sd <- function(sd1, sd2) {((sd1^2 + sd2^2)/2)^0.5}
pooled_sd(all_trials_summary$SD[1], all_trials_summary$SD[2])

# it was brought to my attention that the "effectsize" package has an sd_pooled() function!
# but... that calculates pooled SD weighting the estimate by group sizes
# https://rdrr.io/cran/effectsize/man/sd_pooled.html
# which yields a slightly different answer
# the rstatix and effsize packages will also calculate this for you
# do they give the same answers?

pooledsd1_effectsize_library <- 
  effectsize::sd_pooled(Investigation_Time ~ Genotype, data = hw1_data)
pooledsd1_effectsize_library
pooledsd1  # 14.88 vs 14.43
# Which is "more correct"?

cohend1 <- (all_trials_summary$Mean[1]-all_trials_summary$Mean[2])/pooledsd1
cohend1a <- (all_trials_summary$Mean[1]-all_trials_summary$Mean[2])/pooledsd1_effectsize_library

# the two Cohen's d using pooled SD estimates from the data points are similar
# effectsize::cohens_d(Investigation_Time ~ Genotype, data = hw1_data)
# there is also a cohens_d() function in the rstatix package, used above!
# two cohens_d() functions in two different packages!
# they give different results!! 

cohend2 <- (meanof3_summary$Mean[1]-meanof3_summary$Mean[2])/pooledsd2
# this one uses the SD from averaging all the data across trials
# smaller overall SD with same mean difference gives a much larger effect size
# but this is wrong for the planned study - not averaging multiple points per mouse

pwr.t.test(d=cohend1, sig.level = 0.05, power=0.8, type = "two.sample", alternative = "two.sided")
pwr.t.test(d=cohend1a, sig.level = 0.05, power=0.8, type = "two.sample", alternative = "two.sided")
pwr.t.test(d=cohend2, sig.level = 0.05, power=0.8, type = "two.sample", alternative = "two.sided")

# the first two calculations (using different estimates of effect size) give similar answers

# The third power calculation is wrong, because the SD is artificially  
# deflated from averaging performance across 3 trials for each mouse
# This would be fine if you planned to get 3 trials per mouse in your main study
# But it is inappropriate for estimating the power if you just test each mouse once

##
## what if you wanted to hedge your bets?
## "power to detect a mean difference 20% smaller than in our pilot study"
## look for effect size for mean difference = 19 - (0.2 * 19) = 15.2
##

cohend_alt <- 15.2/pooledsd1
pwr.t.test(d=cohend_alt, sig.level = 0.05, power=0.8, type = "two.sample", alternative = "two.sided")
# need N = 16 per group!
# far more than a 20% increase in N!!

# another defensible option would be to do mean of Cohen's d across each of the 3 trials
# treating each trial as a separate small pilot study

hw1_data %>% group_by(Genotype, Trial) %>%
  summarize(Mean = mean(Investigation_Time),
            SD = sd(Investigation_Time)) %>%
  ungroup() %>% 
  pivot_wider(names_from = Genotype, values_from = c(Mean, SD)) %>%
  mutate(Cohens_d = (Mean_wild_type-Mean_knockout)/pooled_sd(SD_wild_type,SD_knockout))

# can see different estimate of d for each trial
# run that again but extract the mean effect size

mean_effectsize <- 
  hw1_data %>% group_by(Genotype, Trial) %>%
  summarize(Mean = mean(Investigation_Time),
            SD = sd(Investigation_Time)) %>%
  ungroup() %>% 
  pivot_wider(names_from = Genotype, values_from = c(Mean, SD)) %>%
  mutate(Cohens_d = (Mean_wild_type-Mean_knockout)/pooled_sd(SD_wild_type,SD_knockout)) %>%
  select(Cohens_d) %>% summarize(mean(Cohens_d)) %>% pull()

# pull() is useful to grab the number(s) out of a dataframe or tibble

pwr.t.test(d = mean_effectsize, sig.level = 0.05, power = 0.8)
  
# yields (almost) exactly the same answer and same planned sample size

