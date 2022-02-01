library(tidyverse)
library(readxl)

hw1_data <- read_excel("HW1_2022_data.xlsx")
# if your data file is in the "working directory" no need to specify path

# if you have subdirectories the "here" package avoids operating system-specific syntax

hw1_data

# problems to solve:
# need to skip first row
# need to get rid of blank lines / mean calculations
# need to have consistent factor labels for WT and KO
# "tidy" to long format

# skip the first row

hw1_data <- read_excel("HW1_2022_data.xlsx", skip = 1)

hw1_data

# get rid of blank lines and means

hw1_data %>% filter(Genotype %in% c("wt", "WT", "ko", "KO", "k o")) #tedious
hw1_data %>% filter(!is.na(Genotype)) %>% filter(!str_detect(Genotype, "mean"))
hw1_data %>% filter(!is.na(Mouse))

# all of these work
# specific choice may depend on how your excel sheet is formatted

# something like hw1_data <- hw1_data[-c(8, 9, 19), ] to remove problematic rows would also work
# but if you had thousands of rows of data it would be tedious to identify which ones you want to remove
# in this case removing rows with missing values also works (mouse missing in non-data rows)
# hw1_data <- na.omit(hw1_data) or hw1_data %>% drop_na()

hw1_data %>% drop_na()

hw1_data <- 
  hw1_data %>% drop_na()
### update data object!
### the code above produced results but left hw1_data unchanged

# let's deal with the Genotype

table(hw1_data$Genotype)
unique(hw1_data$Genotype)
### these both show you what the codes in Genotype are 
### this is not what we want

### there are a few options
hw1_data %>% mutate(Genotype = factor(Genotype)) %>%
  mutate(Genotype = recode(Genotype, wt = "wild_type", WT = "wild_type",
                           ko = "knockout", KO = "knockout",
                           `k o` = "knockout"))
### note `k o` to deal with that space in there - backwards quotes
### this approach requires listing out values of factor
### two step process of making the factor and then recoding the levels

### could use tolower() to make everything lower case and str_detect_all to find and remove spaces, ...

### could use str_detect from stringr to find W/w and K/k

### or could fix the data before we change it to a factor
hw1_data %>%
  mutate(Genotype = case_when(str_detect(Genotype, "[wW]") ~ "wild_type",
                              str_detect(Genotype, "[kK]") ~ "knockout")) %>%
  mutate(Genotype = factor(Genotype))
### str_detect will check for presence of w or W in string and change variable accordingly
### maybe more general approach than listing options?
### only works this way because the two groups don't share any letters
### can do it inside the mutate too 
hw1_data <- hw1_data %>%
  mutate(Genotype = case_when(str_detect(Genotype, "[wW]") ~ "wild_type",
                              str_detect(Genotype, "[kK]") ~ "knockout") %>% factor())

glimpse(hw1_data)

### Probably OK to have mouse as character variable. If we didn't have 9a in there, it'd get
### read in as numeric though which could cause problems in certain analyses

# let's change data from wide to long

### pivot_longer and pivot_wider change the shape of the data - we want all the times in one
### column and the trial number in other

hw1_data %>% pivot_longer(`Trial 1`:`Trial 3`, names_to = "Trial", 
                          values_to = "Investigation_Time")
### note those backwards quotes again
### we could have renamed those variables or changed names back in the read_excel line
### but it's fine :) 
### good but we might want the Trial as a factor or indeed just as a number

hw1_data <- 
  hw1_data %>% pivot_longer(`Trial 1`:`Trial 3`, names_to = "Trial", 
                          values_to = "Investigation_Time") %>%
  mutate(Trial = factor(parse_number(Trial)))
### parse_number is handy: just takes numeric part of string
### wrapped in factor so Trial ends up a factor variable instead of numeric

hw1_data %>% print(n = Inf)
### instead of 16 lines of data (one per mouse) we have 48 (one per mouse per trial)

# summary statistics!

hw1_data %>% group_by(Genotype, Trial) %>%
  summarize(Mean = mean(Investigation_Time),
            SD = sd(Investigation_Time))

options("digits" = 5)
hw1_data %>% group_by(Genotype, Trial) %>%
  summarize(Mean = mean(Investigation_Time),
            SD = sd(Investigation_Time)) %>% 
  print.data.frame()

### Does this give the same answer as the Excel sheet? (Excel sheet formula is wrong)

### knockout prints before wild_type?
### default is alphabetical order
### can change the order of the factor levels: 

hw1_data %>% mutate(Genotype = factor(Genotype, levels = c("wild_type", "knockout"))) %>%
  group_by(Genotype, Trial) %>%
  summarize(Mean = mean(Investigation_Time),
            SD = sd(Investigation_Time))

# finally, maybe desirable to do this all in one fell swoop

hw1_data <- read_excel("HW1_2022_data.xlsx", skip = 1) %>%
  filter(!is.na(Genotype)) %>% filter(!str_detect(Genotype, "mean")) %>%
  mutate(Genotype = case_when(str_detect(Genotype, "[wW]") ~ "wild_type",
                              str_detect(Genotype, "[kK]") ~ "knockout")) %>%
  mutate(Genotype = factor(Genotype, levels = c("wild_type", "knockout"))) %>%
  pivot_longer(`Trial 1`:`Trial 3`, names_to = "Trial", 
               values_to = "Investigation_Time") %>%
  mutate(Trial = factor(parse_number(Trial)))

# cheeky graph

hw1_data %>%
  ggplot(aes(x = Trial, y = Investigation_Time, color = Genotype)) +
  geom_jitter(width = 0.1) +
  stat_summary(aes(group = Genotype), fun = "mean", geom="line") +
  theme_classic()

           


