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

# lmer

library(lme4)
library(lmerTest)

lmer(Investigation_Time ~ Trial * Genotype + (1|Mouse), data = hw1_data) %>% summary()
lmer(Investigation_Time ~ Trial + Genotype + Trial:Genotype + (1|Mouse), data = hw1_data) %>% summary()
# note that these two are equivalent

lmer(Investigation_Time ~ Trial * Genotype + (1|Mouse), data = hw1_data) %>% anova()

# the random effect of mouse has zero variance
# essemtially, knowing which mouse is which doesn't tell you anything
# there's nothing wrong with this, necessarily, in terms of the random effects
# you get an appropriate null effect of trial out of the anova
# and it still estimates the genotype effect 

lm(Investigation_Time ~ Trial * Genotype, data = hw1_data) %>% summary()
lm(Investigation_Time ~ Trial * Genotype, data = hw1_data) %>% anova()

# note that the summary output gives identical estimates for the coefficients of the fixed effects
# the anova calculation is slightly different for the trial effect only
# (not totally sure why)


# but let's look at a new data set where we do have a trial effect 

## new data set!

hw4_data <- read_excel("HW4_2022_data.xlsx") %>%
  mutate(group = factor(group, levels = c("control", "drug"))) %>%
  pivot_longer(trial_1:trial_4, names_to = "trial", 
               values_to = "preference") %>%
  mutate(trial = factor(parse_number(trial)), subject_ID = factor(subject_ID))

lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data) %>% summary()
# only see trial effect ~ trend at trial 3
# these are comparisons of trials 2, 3, 4 to trial 1

hw4_data_ord <- hw4_data %>% mutate(trial = factor(trial, ordered = TRUE))
lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data_ord) %>% summary()
# ordered factor to get polynomial contrasts - linear contrast is weak
# linear contrast specifies difference between 1 and 2 = diff btw 2 and 3 = diff 3 and 4

lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data) %>% anova()
lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data_ord) %>% anova()
# note: these outputs are identical - polynomial contrasts collapse to same ...
# overall effect of trial 
# robust trial effect - do any trials differ from any other?

lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data) %>% 
  emmeans::emmeans(pairwise ~ trial)
# trial effect strongest from trial 1 to trial 4
# some of effect in individual coefficients diluted by interaction

lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data) %>% 
  emmeans::emmeans(pairwise ~ trial | group)
# interaction not significant, but trial 1 vs 4 only shows up in drug group
# not super strong evidence for differential effect of drug on change across trial

hw4_data %>% ggplot(aes(x=trial,y=preference,color=group)) +
  geom_point() + geom_line(aes(group=subject_ID)) +
  geom_smooth(aes(group=group), method="lm") + theme_classic() +
  scale_color_manual(values = c("deeppink","blue"))

fit_hw1 <- lmer(Investigation_Time ~ Trial * Genotype + (1|Mouse), data = hw1_data)
fit_hw4 <- lmer(preference ~ group * trial + (1|subject_ID), data = hw4_data)  

fixef(fit_hw4) # extract fixed effect coefficients -> regression lines
ranef(fit_hw4) # get estimates of random intercept for each cluster

hw4_data %>% ggplot(aes(x=trial,y=preference,color=subject_ID)) +
  geom_point(size = 2) + geom_line(aes(group=subject_ID),size=0.5) +
  geom_point(aes(y = predict(fit_hw4)),size=2, alpha = 0.5) + 
  geom_line(aes(y = predict(fit_hw4),group=subject_ID),size=1, alpha = 0.5,  linetype = "dashed") + 
  theme_classic() # predicted data from lmer

hw1_data %>% ggplot(aes(x=Trial,y=Investigation_Time,color=Mouse)) +
  geom_point(size = 2) + geom_line(aes(group=Mouse),size=0.5) +
  geom_point(aes(y = predict(fit_hw1)),size=2, alpha = 0.5) + 
  geom_line(aes(y = predict(fit_hw1),group=Mouse),size=1, alpha = 0.5, linetype = "dashed") + 
  theme_classic() # predicted data from lmer
