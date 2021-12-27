library(tidyverse)
library(broom) # useful package for turning model results into a dataframe
data(anscombe) # present in the base R installation

glimpse(anscombe)

ans_data <- list(
  transmute(anscombe, x=x1, y=y1, dataset=1),
  transmute(anscombe, x=x2, y=y2, dataset=2),
  transmute(anscombe, x=x3, y=y3, dataset=3),
  transmute(anscombe, x=x4, y=y4, dataset=4)
)
# this takes each of the x, y pairs in the anscombe data set, codes them as "x" and "y"
# instead of x1, y1 etc, and puts them in a list so that we can neatly run map on it

ans_data

ans_data %>% bind_rows() # all 4 datasets in "long" format

ans_data %>% bind_rows() %>%
  ggplot(aes(x, y)) +
  geom_point() + geom_smooth(method="lm", se=FALSE) + 
  theme_bw() + coord_fixed() +
  facet_wrap(~ dataset)

ans_data %>% map_dfr(~ tidy(lm(y~x, data = .)), .id = "model") # dark magic using purrr


## break down the purrr iteration (optional)

ans_data[[1]] # first item in list

ans_data[[1]] %>% lm(y~x, data = .) # . allows the pipe operator to send to another argument than first

ans_data[[1]] %>% lm(y~x, data = .) %>% tidy() # turns model output into nice data frame

# purrr iterates a function over the elements of a list

ans_data %>% map(~ lm(y~x, data = .)) # the tilde is a shortcut for function (x) {}
# this does the job but it is less nice to look at (output is a list)

ans_data %>% map(~ tidy(lm(y~x, data = .))) # wrap in "tidy"
# output is a list. We can use a variant of map to give output as data frame

ans_data %>% map_dfr(~ tidy(lm(y~x, data = .))) # map_dfr = "map dataframe"
ans_data %>% map_dfr(~ tidy(lm(y~x, data = .)), .id = "model") # adds column to specify list position


## consider regression diagnostic plots on each data set

model1 <- lm(y ~ x, data = ans_data[[1]])
plot(model1)

model2 <- lm(y ~ x, data = ans_data[[2]])
plot(model2)

model3 <- lm(y ~ x, data = ans_data[[3]])
plot(model3)

model4 <- lm(y ~ x, data = ans_data[[4]])
plot(model4)
