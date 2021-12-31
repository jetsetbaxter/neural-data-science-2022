# survival analysis from Emily Zabor
# https://github.com/zabore/tutorials/blob/master/survival_analysis_in_r_tutorial.Rmd

library(tidyverse)
library(lubridate)
library(survival)
library(survminer)

# make fake data
set.seed(20180809)
fkdt <- tibble(Subject = as.factor(1:10), 
               Years = sample(4:20, 10, replace = T),
               censor = sample(c("Censor", rep("Event", 2)), 10, 
                               replace = T)) 
# plot with shapes to indicate censoring or event
ggplot(fkdt, aes(Subject, Years)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_point(data = fkdt, 
             aes(Subject, Years, color = censor, shape = censor), 
             size = 6) +
  coord_flip() +
  theme_minimal() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom")

fkdt <- fkdt %>% mutate(censor_01 = ifelse(censor == "Censor", 0, 1))

Surv(fkdt$Years, fkdt$censor_01) # the  + indicates censored event
# "Survival object"

survfit(Surv(Years, censor_01) ~ 1, data = fkdt) %>% summary()
ggsurvplot(survfit(Surv(Years, censor_01) ~ 1, data = fkdt), risk.table = TRUE)
# Kaplan-Meier model - multiplies probabilities - does not estimate curve
# Step down each time an event occurs
# at 10 years, estimate of 41.7% probability of survival
# "naive" estimate of 40% based on excluding censored cases before 10 year point is close but inaccurate
survfit(Surv(Years, censor_01) ~ 1, data = fkdt)$time
survfit(Surv(Years, censor_01) ~ 1, data = fkdt)$surv # extract K-M estimates at points where censor or event occurs

## more complex example 

ggplot(lung, aes(x = time, fill = factor(status))) +
  geom_histogram(bins = 25, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Censored", "Dead")) +
  labs(x = "Days",
       y = "Count")
# note that distribution of survival and censoring times are skewed
# poses additional modeling challenge 

head(lung)
# status: 1 = censored 2 = dead
# survival / Surv() can handle 0/1, 1/2, or TRUE/FALSE

# example date dataset - if you have dates of events and want to convert to survival data
date_ex <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
  )

date_ex

# format dates w lubridate
date_ex <- date_ex %>% 
  mutate(
    sx_date = ymd(sx_date), 
    last_fup_date = ymd(last_fup_date)
  )

date_ex

# calcluate survival times. %--% creates time interval, as.duration converts to seconds
# dyears(1) gives duration object, number of seconds in 1 year
dyears(1)

date_ex %>% 
  mutate(
    os_yrs = 
      as.duration(sx_date %--% last_fup_date) / dyears(1)
  )


## example with lung data

# survival object + indicates censored from survival package
Surv(lung$time, lung$status)[1:10]

# survival curve
f1 <- survfit(Surv(time, status) ~ 1, data = lung)
names(f1) # what's inside

# survival curve ggplot (surv)

ggsurvplot(
  fit = survfit(Surv(time, status) ~ 1, data = lung), 
  xlab = "Days", 
  ylab = "Overall survival probability")
# fit = f1 would work too
# as before, ticks indicate censoring - not death, but lost to followup

fit4 <- survfit(Surv(time, status) ~ sex, data = lung)
# we may include predictors ("covariates") as in other regression analyses 

summary(fit4)

ggsurvplot(
  fit = fit4, 
  xlab = "Days", 
  ylab = "Overall survival probability",
  legend.title = "",
  legend.labs = c("Male", "Female"),
  palette = c("green4", "purple"))

# this is descriptive but what if we want to test for an effect of sex?

# "log-rank" test: nonparametric; works for single variable
survdiff(Surv(time, status) ~ sex, data = lung)


# Cox proportional hazards regression would allow incorporation of multiple factors
# "proportional hazards" : ratio of hazards for any two individuals is constant over time
# if Kaplan-Meier curves cross or one levels off to zero while the other is still declining
# then you are violating the proportional hazards assumption
# based on the K-M plot that does not appear to be the case here

coxph(Surv(time, status) ~ sex, data = lung) %>% summary()
# in this data set, male coded 1, female coded 2
# output is *hazard* ratio: exponentiated coefficient
# here it is about 0.6
# about 0.6 females dying to every 1 male that dies at any given time point
# not the same as "risk" - hazard is instantaneous, risk/odds is over a period of time

rotorod <- readxl::read_excel("rotorod_surv_example.xlsx")
glimpse(rotorod)
# normally event is "1" when it is measured and "0" when it is not i.e. censored so we will convert "outcome" to 1/0

rotorod <- rotorod %>% mutate(outcome_cens = case_when(outcome == "removed" ~ 0, outcome == "fell" ~ 1))

ggsurvplot(fit = survfit(Surv(time, outcome_cens) ~ 1, data = rotorod), 
           xlab = "Seconds",
           ylab = "Proportion remaining on rotorod")

ggsurvplot(fit = survfit(Surv(time, outcome_cens) ~ sex, data = rotorod), 
           xlab = "Seconds",
           ylab = "Proportion remaining on rotorod")

ggsurvplot(fit = survfit(Surv(time, outcome_cens) ~ genotype, data = rotorod), 
           xlab = "Seconds",
           ylab = "Proportion remaining on rotorod")

ggsurvplot(fit = survfit(Surv(time, outcome_cens) ~ sex + genotype, data = rotorod), 
           xlab = "Seconds",
           ylab = "Proportion remaining on rotorod")

coxph(Surv(time, outcome_cens) ~ sex, data = rotorod) %>% summary()
# at any time, about 5x as many males fallen off as females

f1 <- coxph(Surv(time, outcome_cens) ~ sex * genotype, data = rotorod)
summary(f1)
# consistent with individual survival plots
# females stay on rotorod longer than males on average
# shows crossover interaction: KO males < WT males but KO females > WT females 
# interpretation of coefficients is complicated - sex coefficient is modulated by interaction

cox.zph(f1) # test proportional hazards assumption of Cox regression
cox.zph(f1) %>% plot() # like regression diagnostic plot 
