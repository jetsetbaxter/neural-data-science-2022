library(tidyverse)
library(readxl)

demo <- read_excel("20220126_demo.xlsx")
# requires the excel file be in your working directory

demo
glimpse(demo) # glimpse is tidyverse function that provides compact data view
# extra header column is in the way

demo <- read_excel("20220126_demo.xlsx", skip = 1)

glimpse(demo)
# that's better!

# column names have spaces in them which requires using single quotes
# there are a few options:

# rename function
demo %>% rename(animal_ID = `animal ID`,
                trial_1 = `trial 1`,
                trial_2 = `trial 2`,
                trial_3 = `trial 3`,
                trial_4 = `trial 4`)

# clean_names() function from janitor package
demo %>% janitor::clean_names()

# specify the names when you read the data in 
read_excel("20220126_demo.xlsx", skip = 2, 
                   col_names = c("animal_ID", "trial_1", "trial_2", "trial_3", "trial_4"))

glimpse(demo)
# none of the things we just did had any effect on "demo"

demo <- demo %>% rename(animal_ID = `animal ID`,
                        trial_1 = `trial 1`,
                        trial_2 = `trial 2`,
                        trial_3 = `trial 3`,
                        trial_4 = `trial 4`)

glimpse(demo) # have to remember to assign the result of functions to an object

# note all columns are type "dbl" - double-precision decimals
# read_excel tries to guess what things should be
# you may want animal_ID to be something non-numeric because mouse 2 is not twice mouse 1

demo %>% mutate(animal_ID = factor(animal_ID))

demo

demo <- demo %>% mutate(animal_ID = as.character(animal_ID))

demo
# assigned the result of the second step to demo

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
# assign tidy format data to demo

demo %>% group_by(trial) %>% summarize(mean = mean(latency))

demo %>% group_by(trial) %>% summarize(mean = mean(latency), sd = sd(latency))
# tidy format is handy for generating summary statistics
# better than typing mean(trial_1), mean(trial_2), ...
