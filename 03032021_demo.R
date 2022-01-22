library(tidyverse)

set.seed(1234)

neurons <- read.delim("Neurons.txt", sep = ",", header = FALSE)
conditions <- read.delim("Conds.txt", header = FALSE)
# 3 neurons sampled on each of 900 trials

trial <- 1:nrow(neurons)

decoding_data <- tibble(trial = trial, neuron1 = neurons$V1,
                        neuron2 = neurons$V2, neuron3 = neurons$V3,
                        condition = conditions$V1) %>%
  mutate(condition = factor(condition))

# plot their mean response to each of the conditions
# as usual, there are many ways you could do this

decoding_data %>% pivot_longer(neuron1:neuron3, values_to = "response", names_to = "neuron") %>%
  ggplot(aes(x = condition, y = response, color = neuron, group = neuron)) +
  stat_summary(fun = "mean", geom = "line") +
  scale_color_manual(values = c("red", "green", "purple"))

# examine neurons in 3D feature space
library(plotly)  #install.packages("plotly") ... ggplot does not extend to 3D

fig1 <- 
  decoding_data %>%
  plot_ly(x = ~neuron1, y = ~neuron2, z = ~neuron3, color = ~condition,
          type = "scatter3d", colors = c("red", "green", "purple"), opacity = 0.2)

fig1  # play with it!

decode_means <- decoding_data %>% group_by(condition) %>% 
  summarize(neuron1 = mean(neuron1), neuron2 = mean(neuron2), neuron3 = mean(neuron3))

fig2 <- 
  decode_means %>%
  plot_ly(x = ~neuron1, y = ~neuron2, z = ~neuron3, color = ~condition,
              type = "scatter3d", colors = c("red", "green", "purple"), 
          marker = list(opacity = 1, size = 15))

fig2

subplot(fig1, fig2)  # this took some googling but it's nifty!

# could achieve a similar insight with three 2-D plots?

# stat_summary was doing odd things so I just gave up on that and used decode_means since we had it already
# did you know you could plot new data sets with the same variables on a ggplot coordinate system? now you do!

p1 <- ggplot(decoding_data, aes(x = neuron1, y = neuron2, color = condition)) +
  geom_point(alpha = 0.3) + 
  geom_point(data = decode_means, alpha = 1, size = 5) + theme(legend.position = "none") +
  theme(aspect.ratio=1) + scale_color_manual(values = c("red", "green", "purple"))

p2 <- ggplot(decoding_data, aes(x = neuron1, y = neuron3, color = condition)) +
  geom_point(alpha = 0.3) + 
  geom_point(data = decode_means, alpha = 1, size = 5) + theme(legend.position = "none") +
  theme(aspect.ratio=1) + scale_color_manual(values = c("red", "green", "purple"))

p3 <- ggplot(decoding_data, aes(x = neuron2, y = neuron3, color = condition)) +
  geom_point(alpha = 0.3) + 
  geom_point(data = decode_means, alpha = 1, size = 5) +
  theme(aspect.ratio=1) + scale_color_manual(values = c("red", "green", "purple"))

library(patchwork)   # install.packages("patchwork")

p1 + p2 + p3  # patchwork kind of magic

# anyway that was fun now let's set up the 5-fold cross validation
# I am doing this slightly differently by just setting up a vector with the fold IDs 
# shuffling that and adding it to the dataframe

numtrials <- nrow(conditions)
folds <- 5
which_fold <- rep(seq(1:folds), (numtrials/folds))
# this only works if numtrials is a multiple of folds!
decoding_data$fold <- sample(which_fold, size = length(which_fold), replace = FALSE)

decoding_data
table(decoding_data$fold) # just convince yourself that this worked
decoding_data %>% filter(fold == 1)

predicted_class <- vector(mode = "list", length = folds) # make empty lists to hold these
actual_class <- vector(mode = "list", length = folds) # could use a matrix I guess :p
accuracy <- rep(0, folds)
num_categories <- nlevels(decoding_data$condition)
cat_accuracy <- matrix(0, nrow = folds, ncol = num_categories)

for (k in 1:folds) {
  train <- decoding_data %>% filter(fold != k) # training data are everything but kth fold
  test <- decoding_data %>% filter(fold == k) # hold out data in kth fold
  fit <- MASS::lda(condition ~ neuron1 + neuron2 + neuron3, data = train) # like a regression!
  # lda function comes from MASS package .. install.packages("MASS")
  predicted_class[[k]] <- predict(fit, newdata = test)$class
  actual_class[[k]] <- test$condition
  accuracy[k] <- sum(predicted_class[[k]] == actual_class[[k]])/length(predicted_class[[k]])
  for (cat in 1:num_categories) {
    cat_accuracy[k, cat] <- sum(predicted_class[[k]][actual_class[[k]] == cat] == cat)/
      length(predicted_class[[k]][actual_class[[k]] == cat])
  }
}


accuracy

print(paste0("Mean accuracy is ",round(mean(accuracy),3)))
print(paste0("Chance accuracy is ", round((1/nlevels(decoding_data$condition)),3)))

cat_accuracy # by fold
colMeans(cat_accuracy)

for (k in 1:num_categories) {
  print(paste0("Mean accuracy for category ", k, " is ", round(colMeans(cat_accuracy)[k],3)))
}

# confusion matrix
table(unlist(actual_class), unlist(predicted_class))

table(unlist(actual_class), unlist(predicted_class)) %>% 
  heatmap(Colv = NA, Rowv = NA, col = colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(256))
# could make this nicer with numerical scale etc with some manipulation of the table into a data frame

# this is built into packages for machine learning

library(caret) #install.packages("caret", dependencies = TRUE)
library(MASS) # we need lda so we'll just load MASS
# I usually avoid loading MASS because it overwrites some tidyverse functions

set.seed(1234)

train_control <- trainControl(method = "cv",
                             number = 5,
                             classProbs = TRUE,
                             savePredictions = "final")

lda_neurons_5_cv <- train(
  condition ~ neuron1 + neuron2 + neuron3,
  data = decoding_data,
  method = "lda",
  trControl = train_control
)

# for crying out loud

decoding_data2 <- decoding_data %>%
  mutate(condition = recode_factor(decoding_data$condition, "1" = "Cond1", "2" = "Cond2", "3" = "Cond3"))

lda_neurons_5_cv <- train(
  condition ~ neuron1 + neuron2 + neuron3,
  data = decoding_data2,
  method = "lda",
  trControl = train_control
)

lda_neurons_5_cv

confusionMatrix(lda_neurons_5_cv) # fun :)
