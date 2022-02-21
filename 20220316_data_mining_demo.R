library(tidyverse)

set.seed(1234)

pattern1 <- c(5, 5, 10, 10, 5, 5, 10, 10)

var <- 1

pop1 <- matrix(rep(0, 80), nrow = 10, ncol = 8) # initialize blank matrix

for (k in 1:10) {
  for (j in 1:(length(pattern1))) {
    pop1[k,j] <- pattern1[j] + rnorm(1, 0, var)
  }
}

heatmap(pop1, Colv = NA, Rowv = NA)

pop1_pca <- prcomp(pop1, center = FALSE) # note by default matlab "centers" data R does not
# so scale of values differs between matlab and R

pop1_pca
plot(pop1_pca) # all the variance is on the first component!
str(pop1_pca)

plot(seq(1:8), pop1_pca$rotation[, 1]) # regains pattern from data
# sign and scaling are arbitrary

pop1_pca$x
# scores indicate all observations roughly equally contributing to first PC

pattern2 <- c(5, 10, 5, 10, 5, 10, 5, 10)
pattern3 <- c(5, 5, 5, 5, 10, 10, 10, 10)

pop2 <- matrix(rep(0, 80), nrow = 10, ncol = 8)
pop3 <- matrix(rep(0, 80), nrow = 10, ncol = 8)

for (k in 1:10) {
  for (j in 1:(length(pattern1))) {
    pop2[k,j] <- pattern2[j] + rnorm(1, 0, var)
    pop3[k,j] <- pattern3[j] + rnorm(1, 0, var)
  }
}

heatmap(pop2, Colv = NA, Rowv = NA)
heatmap(pop3, Colv = NA, Rowv = NA)

pop <- rbind(pop1,pop2,pop3)

heatmap(pop, Colv = NA, Rowv = NA)

pop_pca <- prcomp(pop, center = FALSE)
plot(pop_pca)

pop_pca$x
# sort of looks like PC1 is soaking up the overall mean
# PC2 differentiates pop1 and pop2 from each other (pop3 has scores close to 0)
# PC3 differentiates pop3 from the other two

# "psych" package has functions for this that are a little friendlier
psych::principal(pop, nfactors = 8, rotate = "none")
# does a better job of recovering the patterns!
# could see from cumulative variance that it tops out at 3 components
psych::principal(pop, nfactors = 3, rotate = "none")

# anyway, on to clustering

pop_cluster <- pop %>% kmeans(centers = 3)

pop_cluster

# correctly identifies clusters - each group of 10 features assigned to clusters

pop_cluster2 <- pop %>% kmeans(centers = 3)

pop_cluster2
# notably, if you do this twice, you get different specific cluster assignments!

clustered_data <- 
  pop %>% data.frame() %>% mutate(cluster = pop_cluster2$cluster)

clustered_data %>% group_by(cluster) %>% summarize_all(mean)

clustered_data %>% group_by(cluster) %>% summarize_all(mean) %>%
  pivot_longer(X1:X8, values_to = "cluster_mean", names_to = "element") %>%
  mutate(element = factor(parse_number(element)), cluster = factor(cluster)) %>%
  ggplot(aes(x = element, y = cluster_mean, color = cluster)) +
  geom_line(aes(group = cluster))

clustered_data %>% group_by(cluster) %>% summarize_all(mean) %>%
  pivot_longer(X1:X8, values_to = "cluster_mean", names_to = "element") %>%
  mutate(element = factor(parse_number(element)), cluster = factor(cluster)) %>%
  ggplot(aes(x = element, y = cluster_mean, color = cluster)) +
  geom_line(aes(group = cluster)) + facet_wrap(~ cluster) # easier to see patterns
