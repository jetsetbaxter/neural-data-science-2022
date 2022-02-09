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

# Part 1: lmer on HW1 data

library(lme4)
library(lmerTest)

lmer(Investigation_Time ~ Trial * Genotype + (1|Mouse), data = hw1_data) %>% summary()
lmer(Investigation_Time ~ Trial + Genotype + Trial:Genotype + (1|Mouse), data = hw1_data) %>% summary()
# note that these two are equivalent: just illustrating "spelling out" the interaction
# what is going wrong?

# new data set!

hw4_data <- read_excel("HW4_2022_data.xlsx") %>%
  mutate(group = factor(group, levels = c("control", "drug"))) %>%
  pivot_longer(trial_1:trial_4, names_to = "trial", 
               values_to = "preference") %>%
  mutate(trial = factor(parse_number(trial)), subject_ID = factor(subject_ID))

hw4_data %>% ggplot(aes(x=trial,y=preference,color=group)) +
  geom_point(alpha = 0.5) + geom_line(aes(group=subject_ID), alpha = 0.5) +
  geom_smooth(aes(group=group), method="lm", size = 2) + theme_classic()

# Part 2: create an lmer to determine the effect of group and trial and their interaction on preference
