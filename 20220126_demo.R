library(tidyverse)
library(readxl)

setwd("~/Desktop")

demo <- read_excel("20220126_demo.xlsx")

glimpse(demo)
# extra header column is in the way

demo <- read_excel("20220126_demo.xlsx", skip = 1)

glimpse(demo)
# that's better!

# column names have spaces in them which requires using single quotes
# there are a few options:

demo %>% rename(animal_ID = `animal ID`,
                trial_1 = `trial 1`,
                trial_2 = `trial 2`,
                trial_3 = `trial 3`,
                trial_4 = `trial 4`)

demo %>% janitor::clean_names()

demo <- read_excel("20220126_demo.xlsx", skip = 2, 
                   col_names = c("animal_ID", "trial_1", "trial_2", "trial_3", "trial_4"))

glimpse(demo)
# note all columns are type "dbl" - double-precision decimals
# read_excel tries to guess what things should be
# you may want animal_ID to be something non-numeric because mouse 2 is not twice mouse 1

demo %>% mutate(animal_ID = factor(animal_ID))

demo

demo <- demo %>% mutate(animal_ID = as.character(animal_ID))

demo

# you may want all the latency data in one column coded by trial
# pivot_longer and pivot_wider

demo %>% pivot_longer(trial_1:trial_4, names_to = "trial", values_to = "latency") %>%
  mutate(trial = parse_number(trial))

demo %>% pivot_longer(trial_1:trial_4, names_to = "trial", values_to = "latency") %>%
  mutate(trial = parse_number(trial)) %>%
  pivot_wider(names_from = "trial", values_from = "latency", names_prefix = "trial_")
# change it right back!
# these are updated versions of "gather" and "spread" commands from earlier tidyverse

demo <- demo %>% pivot_longer(trial_1:trial_4, names_to = "trial", values_to = "latency") %>%
  mutate(trial = parse_number(trial))

demo %>% group_by(trial) %>% summarize(mean = mean(latency))

demo %>% group_by(trial) %>% summarize(mean = mean(latency), sd = sd(latency))

# you may pass these summary statistics directly to ggplot

demo %>% group_by(trial) %>% summarize(mean = mean(latency), sd = sd(latency)) %>%
  ggplot(aes(x = trial, y = mean)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd))

demo %>% group_by(trial) %>% summarize(mean = mean(latency), sd = sd(latency)) %>%
  ggplot(aes(x = trial, y = mean)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.2) +
  theme_classic() + labs(y = "Latency (sec)")

demo %>%
  ggplot(aes(x = trial, y = latency)) +
  geom_line(aes(group = animal_ID), alpha = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(fun = mean, geom = "line") +
  theme_classic()

demo %>% filter(trial < 4) %>%
  ggplot(aes(x = trial, y = latency)) +
  geom_line(aes(group = animal_ID), alpha = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(fun = mean, geom = "line") +
  theme_classic()

demo %>% filter(trial < 4) %>%
  mutate(trial = factor(trial)) %>%
  ggplot(aes(x = trial, y = latency)) +
  geom_line(aes(group = animal_ID), alpha = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(fun = mean, geom = "line") +
  theme_classic()

demo %>% filter(trial < 4) %>%
  mutate(trial = factor(trial)) %>%
  ggplot(aes(x = trial, y = latency)) +
  geom_line(aes(group = animal_ID), alpha = 0.25) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line") +
  theme_classic()
# had to google that error message
