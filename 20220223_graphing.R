library(tidyverse)
library(readxl)

hw4_data <- read_excel("HW4_data.xlsx") %>%
  mutate(group = factor(group, levels = c("control", "drug"))) %>%
  pivot_longer(trial_1:trial_4, names_to = "trial", 
               values_to = "preference") %>%
  mutate(trial = factor(parse_number(trial)), subject_ID = factor(subject_ID))

hw4_data %>% ggplot(aes(x=trial,y=preference,color=group)) +
  geom_point() + geom_line(aes(group=subject_ID)) +
  geom_smooth(aes(group=group), method="lm") # generates summary regression line by group

# maybe we want one line describing all the data
hw4_data %>% ggplot(aes(x=trial,y=preference,color=group)) +
  geom_point() + geom_line(aes(group=subject_ID)) +
  geom_smooth(method="lm")
# what happened?

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group)) +
  geom_point() + geom_line(aes(group=subject_ID)) +
  geom_smooth(aes(group = 1), method="lm")
# geom_smooth can get confused depending on defaults so "group" aesthetic is helpful
# https://ggplot2.tidyverse.org/reference/aes_group_order.html
# note here that "group" can refer to the "group" aesthetic,
# or the "group" variable in the dataframe ... can be confusing!

## plotting means

hw4_means <- hw4_data %>% group_by(group,trial) %>% summarize(pref_mean = mean(preference)) %>% ungroup()

hw4_means %>% ggplot(aes(x = trial, y = pref_mean, color = group)) +
  geom_col()
# stacked bars are default :p

hw4_means %>% ggplot(aes(x = trial, y = pref_mean, color = group)) +
  geom_col(position = "dodge")
# not quite what we want, perhaps

hw4_means %>% ggplot(aes(x = trial, y = pref_mean, color = group, fill = group)) +
  geom_col(position = "dodge")
# this is straightforward but since we are using a modified data frame, cannot show trendlines

# stat_summary allows mapping of functions to geoms
# https://yjunechoe.github.io/posts/2020-09-26-demystifying-stat-layers-ggplot2/

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID)) +
  stat_summary(fun = mean, geom = "col", position = "dodge")
# hmmm
# make bars transparent
# get the lines to line up with the bars

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_dodge(width = 0.7)) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5)
# how to get the trendlines to line up with the bars?

# probably many solutions ...

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug"))
# this required some extensive googling for this specific use case!!
# figured out arguments for "position_nudge" by trial and error 
# change the order of the geoms/stats - get lines on top of bars!

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0)
# remove that border around the bars

# themes and labels

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic()
# removes background grid
# floating bars!

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic() + 
  scale_y_continuous(expand=c(0,0)) # removes default expansion at top and bottom of y axis
# kind of squishes the top a little now

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .05)))
# expand 5% at top of y-axis and 0% at bottom

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .05))) + 
  scale_color_manual(values = c("purple", "orange"))
# oh my
# possible to call colors by name in R
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .05))) + 
  scale_color_manual(values = c("purple", "orange")) +
  scale_fill_manual(values = c("purple", "orange"))
# must give scales for both color and fill aesthetics
# "scale_color" only passes a scale to the color aesthetic

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .05))) + 
  scale_color_viridis_d(end = 0.8) +
  scale_fill_viridis_d(end = 0.8)
# viridis_d - d for "discrete"
# viridis_c - c for "continuous"
# specify "begin" and "end" parameters to set where it is on the scale (end = 1 ugly yellow)

hw4_data %>% ggplot(aes(x=trial,y=preference, color=group, fill = group)) +
  geom_line(aes(group=subject_ID), position = position_nudge(-0.22), 
            data = subset(hw4_data, group == "control")) +
  geom_line(aes(group=subject_ID), position = position_nudge(0.22), 
            data = subset(hw4_data, group == "drug")) +
  stat_summary(fun = mean, geom = "col", position = "dodge", alpha = 0.5, color = 0) +
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, .05))) + 
  scale_color_viridis_d(end = 0.8) +
  scale_fill_viridis_d(end = 0.8) +
  xlab("Trial") + ylab("Preference score") + ggtitle("Homework 4 Data") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))
# I have to google this every time for changing the font and position
# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
# https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2

library(palmerpenguins)

penguins %>% ggplot(aes(x=bill_length_mm, y=bill_depth_mm)) + geom_point(aes(color = species)) +
  geom_smooth(method = "lm")
# in this case, geom_smooth defaults to a line across all the data

penguins %>% ggplot(aes(x=bill_length_mm, y=bill_depth_mm)) + geom_point(aes(color = species)) +
  geom_smooth(aes(color = species), method = "lm")
# if geom_smooth has a color aesthetic, it will draw a line for each group

penguins %>% ggplot(aes(x=bill_length_mm, y=bill_depth_mm, color = species)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# same as before! (except we specified SE = FALSE to remove those CIs)
# the entire ggplot canvas has a color aesthetic
# geom_smooth inherits that aesthetic
# can move aesthetics around depending on how you want graph constructed

# export publication quality graph

dpi <- 600
tiff("output.tif", width=8*dpi, height=5*dpi, res=dpi) # specifies graphics device
# can export to jpg, png, ...
# (sets dpi for appropriate width but graphics program may default to 72 dpi)
# can constrain proportions of graph to get square etc
penguins %>% ggplot(aes(x=bill_length_mm, y=bill_depth_mm, color = species)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)
dev.off() # ends export to graphics device 
# some trial and error involved in this process
# note that TIFF files are enormous!
# https://rforbiochemists.blogspot.com/2015/05/export-high-resolution-graph-for.html

##

## A general tutorial website

# https://github.com/dhmontgomery/nicar20/tree/master/ggplot-graphics

## An interesting specific case

# https://github.com/gruggeri/ADSCV_media/blob/master/tidytuesday/cleaning_tate_artwork_bar.md
