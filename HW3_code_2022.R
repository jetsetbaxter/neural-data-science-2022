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

hw1_data_mean <- hw1_data %>% group_by(Mouse, Genotype) %>% 
  summarize(mean_time_per_mouse = mean(Investigation_Time)) %>%
  ungroup() 

# t-test

t.test(mean_time_per_mouse ~ Genotype, data = hw1_data_mean)

# lm

lm(mean_time_per_mouse ~ Genotype, data = hw1_data_mean) %>% summary()

t.test(mean_time_per_mouse ~ Genotype, data = hw1_data_mean, var.equal = TRUE) # explains difference in results

# genotype and trial

lm(Investigation_Time ~ Trial + Genotype, data = hw1_data) %>% summary()
# automatically produces dummy coded variables for Trial - so you get 2 Trial effects
# you need 2 dummy coded variables to represent 3 levels of Trial
# in general, need N-1 dummy coded variables to represent a category with N levels
# because if it's not in any of the N-1 categories, it has to be the Nth
# these are coefficients of effect relative to "reference level" - in this case Trial 1
# by default, reference level is the first level of the factor
# You can specify the order of factor levels to change these 
# e.g. mutate(Trial = factor(Trial, levels = c(2, 1, 3)))
# now 2 is the first level and will be used as the reference level
# note, specifying order does not produce an "ordered factor" see below

lm(Investigation_Time ~ Trial + Genotype, data = hw1_data) %>% anova()

# what if we had not made Trial a factor?

hw1_data_num <- hw1_data %>% mutate(Trial = as.numeric(Trial))
# Trial is a numeric variable and not a factor now

lm(Investigation_Time ~ Trial + Genotype, data = hw1_data_num) %>% summary()
lm(Investigation_Time ~ Trial + Genotype, data = hw1_data_num) %>% anova()
# Trial has 1 df, being treated as numeric variable
# assumes that effect of Trial is perfectly linear - trial 2 is 2x trial 1, trial 3 is 3x trial 1
# perhaps not desirable

# treat Trial as an ordered factor - specify that 1 < 2 < 3

hw1_data_ord <- hw1_data %>% mutate(Trial = factor(Trial, levels = c(1,2,3), ordered=TRUE))
# "ordered=TRUE" produces ordered factor which generates different dummy coding
# again, specifying a specific order to factor levels does not produce an "ordered" factor unless you specify it

lm(Investigation_Time ~ Trial + Genotype, data = hw1_data_ord) %>% summary()
lm(Investigation_Time ~ Trial + Genotype, data = hw1_data_ord) %>% anova()

# produces polynomial contrasts by default
# anova output is the same, but summary output breaks contrasts up a different way

## graphs to think about how this works

hw1_data %>% ggplot(aes(x = 2, y = Investigation_Time)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0.7,3.3)) + 
  stat_summary(geom = "errorbar", fun.min = mean, 
               fun = mean, fun.max = mean, width = .75, color = "blue")

hw1_data %>% ggplot(aes(x = 2, y = Investigation_Time, color = Genotype)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0.7,3.3)) + 
  stat_summary(geom = "errorbar", fun.min = mean, 
               fun = mean, fun.max = mean, width = .75)

hw1_data %>% ggplot(aes(x = Trial, y = Investigation_Time, color = Genotype)) + 
  geom_point() + 
  stat_summary(geom = "errorbar", fun.min = mean, 
               fun = mean, fun.max = mean, width = .75)
