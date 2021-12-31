library(tidyverse)
library(lme4)
library(lmerTest)

set.seed(123)

n<-8

# simulate an appropriate multivariate normal data set: uses mvrnorm function from MASS package
# skip over this unless you're interested in details of data simulation

mu_control = rep(0, 4)
mu_exp = rep(0.8, 4)
#creates an appropriately-sized correlation matrix based on the number of trials you've selected
matrix = c()
long_matrix = c()
for( rownumber in 1:4 )  {
  row = rep(0.35, 4)
  row[rownumber] = 1
  long_matrix = c(long_matrix, row)
}
matrix = matrix(long_matrix, nrow = 4)
control_data = MASS::mvrnorm(n = n, mu = mu_control, Sigma = matrix, empirical = FALSE)
control_data = control_data %>% as.data.frame() %>% mutate(group = "control", subject_ID = 1:n)
exp_data = MASS::mvrnorm(n = n, mu = mu_exp, Sigma = matrix, empirical = TRUE)
exp_data = exp_data %>% as.data.frame() %>% mutate(group = "exp", subject_ID = 1+n:(n*2-1))
wide_data = rbind(control_data, exp_data)
end_column = paste("V", 4 %>% as.character(), sep = "")

long_data <- wide_data %>% 
  pivot_longer(cols = V1:end_column, names_to = "trial", names_prefix = "V")
long_data <- long_data %>% 
  mutate(trial = trial %>% as.ordered(), # as.ordered() creates *ordered factor*
         group = group %>% as.factor(), 
         subject_ID = subject_ID %>% as.factor())

long_data$trial  # note the levels of the factor are 1 < 2 < 3 < 4

long_data <- long_data %>% mutate(value = value + 0.3*(as.numeric(trial)-1))
# add a trial effect into simulated data

long_data %>% ggplot(aes(x=trial,y=value,color=group)) + geom_line(aes(group=subject_ID)) +
  stat_summary(aes(group = group, color = group), fun = "mean", geom = "line", size = 3)

lm(value ~ group + trial, data = long_data) %>% anova() # p-values are *incorrect*
# this formulation does not take into account dependence within subjects of trial effect
# estimates of variance components are wrong 

lmer(value ~ group + trial + (1|subject_ID), data = long_data) %>% summary()
lmer(value ~ group + trial + (1|subject_ID), data = long_data) %>% anova()
# group effect is the same in ANOVA and model summary
# model summary is estimating separate coefficients for each polynomial contrast 
# ANOVA trial effect collapses 3 separate contrasts into 1 F test
# is there any difference between any of the trials with respect to each other
# "trial.L" "trial.Q" "trial.C" are linear, quadratic, cubic contrasts
# different approach than dummy coding trials 2, 3, 4 relative to trial 1
# even though that would also work and would give same overall effect in ANOVA

long_data %>% mutate(trial = as.character(trial)) %>%
  lmer(value ~ group + trial + (1|subject_ID), data = .) %>% summary()
long_data %>% mutate(trial = as.character(trial)) %>%
  lmer(value ~ group + trial + (1|subject_ID), data = .) %>% anova()
# just a different way of decomposing the trial effect 

# do polynomial contrasts manually 

long_data <- long_data %>% 
  mutate(linear_trend = case_when(trial == 1 ~ -3,
                                  trial == 2 ~ -1,
                                  trial == 3 ~ 1,
                                  trial == 4 ~ 3),
         quad_trend = case_when(trial == 1 ~ -1,
                                trial == 2 ~ 1,
                                trial == 3 ~ 1,
                                trial == 4 ~ -1),
         cubic_trend = case_when(trial == 1 ~ -1,
                                 trial == 2 ~ 3,
                                 trial == 3 ~ -3,
                                 trial == 4 ~ 1))

lm(value ~ group + linear_trend + quad_trend + cubic_trend, data = long_data) %>% summary()
lm(value ~ group + trial, data = long_data) %>% summary()
# our constructed contrasts are the same as what R does under the hood
# the coefficients for the trends are the same but the p-values are off because
# we are not accounting for the dependence between the data points

# p-value for group effect is off - df are inflated because quadruple-counting subjects

long_data %>% group_by(subject_ID, group) %>% summarize(mean_value = mean(value))

long_data %>% group_by(subject_ID, group) %>% 
  summarize(mean_value = mean(value)) %>% ungroup() %>%
  lm(mean_value ~ group, data = .) %>% summary()
# now you get the same p-value you do out of the lmer

##

## build in an interaction effect

##

long_data_2 <- long_data %>% 
  mutate(value = value + 0.5*(as.numeric(trial)-1)*as.numeric(group == "exp"))

long_data_2 %>% ggplot(aes(x=trial,y=value,color=group)) + geom_line(aes(group=subject_ID)) +
  stat_summary(aes(group = group, color = group), fun = "mean", geom = "line", size = 3)

lmer(value ~ group + trial + (1|subject_ID), data = long_data_2) %>% summary()

lmer(value ~ group * trial + (1|subject_ID), data = long_data_2) %>% summary()
# interaction reflects difference in trial effect depending on level of group
lmer(value ~ group * trial + (1|subject_ID), data = long_data_2) %>% anova()
# p-value for group is the same
# trial and trial x group are collapsed into 3 df effects
# obscures interaction of group with linear trend of trial

##

## missing data !!

##

long_data_3 <- long_data_2
long_data_3$value[long_data_3$subject_ID == 3 & long_data_3$trial == 2] <- NA
long_data_3$value[long_data_3$subject_ID == 14 & long_data_3$trial == 4] <- NA
# delete 2 values

long_data_3 %>% ggplot(aes(x=trial,y=value,color=group)) + geom_line(aes(group=subject_ID)) +
  stat_summary(aes(group = group, color = group), fun = "mean", geom = "line", size = 3)

lm(value ~ group + linear_trend + quad_trend + cubic_trend, data = long_data_3) %>% summary()
# illustrates casewise deletion (although this is the wrong analytic approach anyway)

lmer(value ~ group * trial + (1|subject_ID), data = long_data_3) %>% summary()

##

## marginal means

##

library(emmeans)

# https://timmastny.rbind.io/blog/tests-pairwise-categorical-mean-emmeans-contrast/

ld3model <- lmer(value ~ group * trial + (1|subject_ID), data = long_data_3)
summary(ld3model)
anova(ld3model)

ld3model %>% emmeans(pairwise ~ group)

ld3model %>% emmeans(consec ~ trial)

ld3model %>% emmeans(consec ~ trial | group)

ld3model %>% emmeans(consec ~ trial | group, adjust = "none")

##

## hierarchical comparisons

##

ld3model <- lmer(value ~ group * trial + (1|subject_ID), data = long_data_3)
ld3model_reduced <- lmer(value ~ group + trial + (1|subject_ID), data = long_data_3) # main effects, no interactions

anova(ld3model, ld3model_reduced)

# inclusion of interaction term significantly improves model fit
# requires same random effects structure
# REML vs FIML complex - FIML generates smaller estimates of variance but they are potentially biased (overfit)
# hence p-value for this comparison is a little smaller than that in model ANOVA table

ld3model <- lmer(value ~ group * trial + (1|subject_ID), data = long_data_3)
ld3model_reduced2 <- lmer(value ~ trial + (1|subject_ID), data = long_data_3) # effect of trial only

anova(ld3model, ld3model_reduced2)
# 4 df: effect of group (1) and group x trial (3)
# does adding group to the model have any influence at all on prediction? 
# you may not be interested on whether an effect interacts with trial or whether just a main effect






