library(tidyverse)
library(lme4)  # install.packages(c("lme4", "lmerTest"))
library(lmerTest) # lmerTest will give you p values 

babyrats <- readRDS("babyrats.rds")

glimpse(babyrats)

t.test(open_arms ~ iso, data = babyrats)
# does not account for dependence

babyrats %>% ggplot(aes(x=iso,y=open_arms)) + 
  geom_jitter(aes(color=sex,shape=litter), width=0.1, size=2) +
  geom_bar(position="dodge", stat = "summary", fun = "mean", alpha = 0.2)

# main effect of isoflurane
lmer(open_arms ~ iso + (1|litter), data = babyrats) %>% summary()
lmer(open_arms ~ iso + (1|litter), data = babyrats) %>% anova()

# effects of both isoflurane and sex
lmer(open_arms ~ iso + sex + (1|litter), data = babyrats) %>% summary()
lmer(open_arms ~ iso + sex + (1|litter), data = babyrats) %>% anova()

# interaction of isoflurane and sex!?!!??!
lmer(open_arms ~ iso * sex + (1|litter), data = babyrats) %>% summary()
lmer(open_arms ~ iso * sex + (1|litter), data = babyrats) %>% anova()
# difference between P values with type 1 and 3 sums of squares!
# summary is giving type 1 sums of squares
# anova, for lmer objects, defaults to type 3

# we will talk about what might be better procedures for estimating
# p-values for higher-order effects in the context of 
# hierarchical models soon
