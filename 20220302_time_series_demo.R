library(tidyverse)

LFP <- read.delim("LFP.txt", header = FALSE)
events <- read.delim("events.txt", header = FALSE)

glimpse(LFP)
glimpse(events) # just vectors labeled V1

LFP_data <- tibble(LFP = LFP$V1, time = 1:length(LFP))
LFP_data <- LFP_data %>% mutate(event = time %in% events$V1)
# creates a 0/1 flag for whether a time point is associated with an event

glimpse(LFP_data)

ggplot(LFP_data, aes(x = time, y = LFP)) + geom_line(alpha = 0.7) +
  geom_vline(aes(xintercept = time), data = subset(LFP_data, event == TRUE), color = "red", alpha=0.4)
# see increase in LFP signal during period when there are no events

# separate data into trials, 3000 ms around each event

trials <- matrix(0, nrow = length(events$V1), ncol = 3000)
# empty matrix with 1 row per event and 3000 columns

for (k in 1:length(events$V1)) {
  trials[k,] <- LFP_data$LFP[(events$V1[k]-999):(events$V1[k]+2000)]
}
# uses numerical values from events vector - in events$V1 - to set index for retrieving LFP data
# and placing in matrix
# goes from 999 ms before event to 2000 ms after

trials %>% as.data.frame() %>% glimpse()

trials_aligned <- 
  trials %>% as.data.frame() %>% pivot_longer(V1:V3000, names_to = "time_point", values_to = "LFP") %>%
  mutate(time_point = parse_number(time_point)-1000) %>%
  mutate(trial = sort(rep(1:nrow(trials), ncol(trials))))
# takes matrix of trials, puts in long format, adds trial indicator

# heatmap

trials_aligned %>%
  ggplot(aes(x = time_point, y = trial, color = LFP)) +
  geom_tile() + # scale_color_viridis_c() +
  scale_color_gradientn(colors = pals::parula()) +
  geom_vline(xintercept = 0, color = "red") +
  scale_x_continuous(breaks = seq(-1000, 2000, 500)) +
  scale_y_reverse(breaks = seq(50,500,50)) +
  xlab("Time from event (ms)") + ylab("Trials") +
  theme_bw()
# "pals" package contains "parula" colors which is matlab default
# viridis, which is in ggplot2, is close

# mean response across trials

trials_aligned %>%
  ggplot(aes(x = time_point, y = LFP)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 0, color = "red") +
  xlab("Time from event (ms)") + ylab("Average event-related response")

# smooth original data: requires smooth moving average (SMA) function from TTR package

smooth_LFP <- TTR::SMA(LFP, n = 100)

# repeat same as before
sm_LFP_data <- tibble(smooth_LFP = smooth_LFP, time = 1:length(smooth_LFP))
sm_LFP_data <- sm_LFP_data %>% mutate(event = time %in% events$V1) %>% filter(!is.na(smooth_LFP))
# note that first 99 values in smooth_LFP are NA

smooth_trials <- matrix(0, nrow = length(events$V1), ncol = 3000)
# empty matrix with 1 row per event and 3000 columns

for (k in 1:length(events$V1)) {
  smooth_trials[k,] <- sm_LFP_data$smooth_LFP[(events$V1[k]-999):(events$V1[k]+2000)]
}
# uses numerical values from events vector - in events$V1 - to set index for retrieving LFP data
# and placing in matrix

smooth_trials %>% as.data.frame() %>% glimpse()

# mean response across trials: as before (reformat data to as "trials_aligned")
smooth_trials %>% as.data.frame() %>% pivot_longer(V1:V3000, names_to = "time_point", values_to = "LFP") %>%
  mutate(time_point = parse_number(time_point)-1000) %>%
  ggplot(aes(x = time_point, y = LFP)) +
  stat_summary(fun = mean, geom = "line") +
  geom_vline(xintercept = 0, color = "red") +
  xlab("Time from event (ms)") + ylab("Average event-related response")
