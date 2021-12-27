library(tidyverse)
library(palmerpenguins)

glimpse(penguins)
# 5 numeric variables (bill length, bill depth, flipper length, body mass, year)
# 3 categorical variables (species, island, sex)

t.test(body_mass_g ~ sex, data = penguins)

lm(body_mass_g ~ sex, data=penguins) %>% summary()
# [almost] same result as t test (why? ... t.test does not assume equal variance by default)
# you need to pass model object to "summary" to see results
# "sexmale" - how much prediction of body mass is adjusted from intercept
# for an observation for which sex == male
# that value is 683.41 - can you see where it comes from?
# "reference level" of sex effect is female (default is alphabetical)
penguins$sex # lists observations, levels at end - shown in order


t.test(body_mass_g ~ species, data = penguins)
# there are 3 different species so a t-test will not work

lm(body_mass_g ~ species, data = penguins) %>% summary()
# there are 3 species
penguins$species
levels(penguins$species) # less tedious
# by default, lm will "dummy code" the first level of a factor as the "reference level"
# so "speciesChinstrap" and "speciesGentoo" are how much you would adjust your prediction
# of body mass relative to what you would predict for a Adelie penguin
# what do the coefficient estimates mean?
# here's a hint ...
# penguins %>% group_by(species) %>% summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))

lm(body_mass_g ~ species, data = penguins) %>% summary()
lm(body_mass_g ~ species, data = penguins) %>% anova()
# look at the similarities and differences in the output from passing the 
# lm model to "summary" versus "anova"
# for an ANOVA, the test is whether any of the levels differ from each other
# sometimes this is the question
# the evaluation of the p-values gets complicated quickly when there are
# many variables in the analysis so this requires caution

lm(body_mass_g ~ species, data = penguins) %>% summary()
lm(body_mass_g ~ sex, data = penguins) %>% summary()
lm(body_mass_g ~ species + sex, data = penguins) %>% summary()
# notice that the coefficient estimates when both variables are in the model differ slightly
# why might this be?
# hint: how many missing observations?

penguins_complete <- penguins %>% filter(!is.na(sex))
lm(body_mass_g ~ species, data = penguins_complete) %>% summary()
lm(body_mass_g ~ sex, data = penguins_complete) %>% summary()
lm(body_mass_g ~ species + sex, data = penguins_complete) %>% summary()
# still differ slightly - consider table(penguins_complete$species, penguins_complete$sex)
# species and sex are not independent of one another

penguins %>% group_by(species) %>% summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))
penguins %>% group_by(sex) %>% summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))


## inclusion of interaction term

lm(body_mass_g ~ species + sex, data = penguins) %>% summary()
# effect of sex is modeled independently of effect on species
# intercept is for both reference levels (Adelie, female)
# adjust a fixed amount from intercept if Chinstrap or Gentoo
# adjust another fixed amount if male

lm(body_mass_g ~ species * sex, data = penguins) %>% summary()
# make additional adjustments for male Chinstrap or male Gentoo

penguins %>% filter(!is.na(sex)) %>% group_by(species, sex) %>% 
  summarize(mean_mass = mean(body_mass_g, na.rm = TRUE))
# notice that difference between female and male Chinstraps is smaller than Adelie
# difference between female and male Gentoos is larger than Adelie

penguins %>% ggplot(aes(x = species, y = body_mass_g)) + stat_summary(fun = "mean", geom = "bar")
penguins %>% filter(!is.na(sex)) %>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) + 
  stat_summary(fun = "mean", geom = "bar", position = "dodge")


## consider a continuous interaction effect

lm(bill_length_mm ~ bill_depth_mm, data = penguins) %>% summary()
plot(penguins$bill_depth_mm, penguins$bill_length_mm)
penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) + geom_point(size = 2, alpha = 0.6)
# bills with more depth seem to be shorter?

# we can explore aspects of this relationship
penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species)) + geom_point(size = 2, alpha = 0.6)
# the relationship between length and depth is strongly mitigated by species
# "Simpson's Paradox"

lm(bill_length_mm ~ bill_depth_mm + species, data = penguins) %>% summary()
add_model <- lm(bill_length_mm ~ bill_depth_mm + species, data = penguins)

coef(add_model)
coef(add_model)[[1]]

penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species)) + 
  geom_point(size = 2, alpha = 0.6) +
  geom_abline(slope = coef(add_model)[[2]], 
              intercept = coef(add_model)[[1]], color = scales::hue_pal()(3)[1]) +
  geom_abline(slope = coef(add_model)[[2]], 
              intercept = (coef(add_model)[[1]] + coef(add_model)[[3]]), color = scales::hue_pal()(3)[2]) +
  geom_abline(slope = coef(add_model)[[2]], 
              intercept = (coef(add_model)[[1]] + coef(add_model)[[4]]), color = scales::hue_pal()(3)[3])
# note that the lines are parallel (they all have the same slope)
# the scales package has a function to generate the ggplot2 colors
# which I totally just googled the moment that I needed it

lm(bill_length_mm ~ bill_depth_mm * species, data = penguins) %>% summary()
# significant interaction coefficients
int_model <- lm(bill_length_mm ~ bill_depth_mm * species, data = penguins)
coef(int_model)
coef(int_model)[6]

# the interaction coefficients are a modifier on the slope
# so for Adelie, slope is bill_depth_mm 
# for Chinstrap, slope is bill_depth_mm + bill_depth_mm:speciesChinstrap
# for Gentoo, slope is bill_depth_mm + bill_depth_mm:speciesGentoo
# presence of interaction term modifies both intercept *and slope*

penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species)) + 
  geom_point(size = 2, alpha = 0.6) +
  geom_abline(slope = coef(int_model)[[2]], 
              intercept = coef(int_model)[[1]], color = scales::hue_pal()(3)[1]) +
  geom_abline(slope = (coef(int_model)[[2]] + coef(int_model)[[5]]), 
              intercept = (coef(int_model)[[1]] + coef(int_model)[[3]]), color = scales::hue_pal()(3)[2]) +
  geom_abline(slope = (coef(int_model)[[2]] + coef(int_model)[[6]]), 
              intercept = (coef(int_model)[[1]] + coef(int_model)[[4]]), color = scales::hue_pal()(3)[3])

# there is a less tedious way
penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm, color = species)) + 
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)

# just notice how different this relationship appears if you do not consider species
penguins %>% ggplot(aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE)

library(car)

lm(bill_length_mm ~ bill_depth_mm * species, data = penguins) %>% summary()
lm(bill_length_mm ~ bill_depth_mm * species, data = penguins) %>% anova()

lm(bill_length_mm ~ bill_depth_mm * species, data = penguins) %>% Anova(type=3) # capital A Anova in car package
# oh no what is happening
# this is the difference between type 1 and type 3 sums of squares
# if you get an anova summary table with an lm object, default is type 1 SS
# these are calculated separately for main effects and interactions
# so there is an overall difference between species and and overall effect of bill depth
# then it separately evaluates the interaction term
# type 3 SS evaluate all simultaneously (default in many statistical packages)
# the interaction between species and bill depth captures much of the variance in species
# as we can see in the summary of the regression output (coef of species not significant on their own)
# type 1 SS essentially evaluating species + species:bill_depth for overall species effect
# all this is to say just be careful if you want an ANOVA output from a model in R
# there are other ANOVA functions and it is important to be sure what you are getting



