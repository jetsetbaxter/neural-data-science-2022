library(tidyverse)
library(lme4)
library(lmerTest)

rbinom(1, 1, 0.5)    #1 flip of 1 coin with heads probability 0.5
rbinom(10, 1, 0.5)   #10 flips of 1 coin with heads probability 0.5
rbinom(10, 10, 0.5)  #10 flips each of 10 coins each with heads probability 0.5

# a coin flip is a Bernouilli trial

set.seed(123)

control <- replicate(10, rbinom(20, 1, 0.6)) %>% t()
drug <- replicate(10, rbinom(20, 1, 0.75)) %>% t()

#t() is for "transpose"

control # produces an array!
# each row is a mouse, each column is the outcome of a trial 0 or 1

data.frame(control)

recog_data <- rbind(data.frame(control), data.frame(drug)) %>%                # stack 'em up
  tibble() %>%                                                                # convert to tibble
  mutate(subject_id = factor(seq(1:20)),                                      # make a variable for subject 1-20
         condition = factor(c(rep("control",10),rep("drug",10)))) %>%         # and condition 10 controls, 10 drugs
  pivot_longer(X1:X20, names_to = "trial", values_to = "trial_outcome") %>%   # make long format
  mutate(trial = factor(parse_number(trial)))                                 # convert trial to factor

recog_data

recog_data %>% group_by(condition) %>% summarize(mean(trial_outcome))
# 61% in control, 75% in drug

recog_data %>% group_by(subject_id, condition) %>% summarize(mean_trial = mean(trial_outcome)) %>% print(n = Inf)

mouse_means <- recog_data %>% group_by(subject_id, condition) %>%
  summarize(mean_trial = mean(trial_outcome)) %>% ungroup()

t.test(mean_trial ~ condition, data = mouse_means)

t.test(mean_trial ~ condition, data = mouse_means, var.equal = TRUE)

lm(mean_trial ~ condition, data = mouse_means) %>% summary()

lm(trial_outcome ~ condition, data = recog_data) %>% summary()
# coefficients are the same, p-values are different (slightly)
# df are higher because we are analyzing 400 trials instead of 20 means

glm(trial_outcome ~ condition, data = recog_data, family = binomial()) %>% summary()
# the coefficients are different
# this analysis produces "log odds" - the output of the logit function
# how do we get the probability back?

glm(trial_outcome ~ condition, data = recog_data, family = binomial()) %>% 
  coefficients() %>%
  plogis()  # inverse logit function

# we see 0.61 again!
# what is 0.657?
# odds ratio (coefficient is log odds ratio)

# intercept is the same
# interpretation of coefficient for drug?
# negative means it's decreasing the odds, positive means it's increasing the odds
# coefficients are logs of odds ratios (logs add, odds are multiplicative)
# to regenerate, add coefficient to intercept and take inverse logit

glm(trial_outcome ~ condition, data = recog_data, family = binomial()) %>% 
  coefficients() %>%
  sum() %>%
  plogis()

glmer(trial_outcome ~ condition + (1|subject_id), data = recog_data, family = binomial()) %>% summary()
# not surprisingly the random effect essentially has zero variance
# all the trials are essentially independent so you get the same result as non-MLM
# because it is operating on the outcome of a *single trial* it does not matter
# that you have 20 trials from each of 10 mice or 1 trial from each of 200


##

## continuous predictor

##

library(palmerpenguins)

penguins %>% group_by(sex) %>% summarize(mean(body_mass_g, na.rm = TRUE))

lm(body_mass_g ~ sex, data = penguins) %>% summary()

lm(sex ~ body_mass_g, data = penguins) %>% summary()
# it does not like that!
# we could convert sex to 0,1 but let's use logistic regression

glm(sex ~ body_mass_g, data = penguins, family = binomial()) %>% summary()

ggplot(penguins, aes(x=body_mass_g, y=(as.numeric(sex)-1))) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se = FALSE) + 
  ylab("P(male)") + xlab("Body mass in g")
# as.numeric(sex)-1 is a trick to get it to generate female = 0 and male = 1
# so that we can use stat_smooth() to draw the logistic regression curve

ggplot(penguins, aes(x=body_mass_g, y=(as.numeric(sex)-1))) + geom_point() + 
  stat_smooth(method="lm", se = FALSE) + 
  ylab("P(male)") + xlab("Body mass in g")
# if we just did linear regression, we see that penguins > ~6050 g have a probability greater than 1 of being male


