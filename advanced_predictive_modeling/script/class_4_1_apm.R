recurence_cluster <- function(n, k) {
  if (k == 1 | k == n)
    return(1)
  return(recurence_cluster(n - 1, k - 1) + k * recurence_cluster(n - 1, k))
}

recurence_cluster(10,4)

library(jpeg)

parrot <- jpeg::readJPEG("advanced_predictive_modeling/data/parrot.jpg")

parrot_dim <- dim(parrot)

parrotRGB <- data.frame(x = rep(1:parrot_dim[2],each = parrot_dim[1]),
                        y = rep(parrot_dim[1]:1, parrot_dim[2]),
                        R = as.vector(parrot[,,1]),
                        G = as.vector(parrot[,,2]),
                        B = as.vector(parrot[,,3]))

library(tidyverse)

parrotRGB %>% 
ggplot() +
  aes(x = x, y = y) +
  geom_point(color = rgb(parrotRGB[c("R","G","B")])) +
  theme_void()

k <- 5
k_means <- kmeans(parrotRGB[,c("R","G","B")],
                  centers = k,
                  nstart = 10)
quantized_colors <- rgb(k_means$centers[k_means$cluster,])


parrotRGB %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_point(color = quantized_colors) +
  theme_void()


db <- read_csv('advanced_predictive_modeling/data/e-commerce.csv')

k <- 7

set.seed(1234)

k_means <- kmeans(db,centers = k,nstart = 100)

k_means$cluster



plot(
  db,
  col = k_means %>% pluck('cluster'),
  main = sprintf('Algoritmo k_medias com %d clusters',k), xlab = 'income', ylab = 'spent', pch = 20, cex = 1.2)


k_range <- 2:15


total_dispersion <- numeric(length(k_range))

load(attitude)
attitude
for (k in 1:length(k_range)) {
  k_means <- kmeans(db,centers = k_range[k],nstart = 100)
  
  total_dispersion[k] <- k_means$tot.withinss
}

plot(k_range,total_dispersion,type = 'b',lwd = 2,col = 'blue', yaxt = 'n',xlab = 'k',ylab = 'total dispersion inter cluster')

db <- attitude[,3:4]

k <- 6

set.seed(1234)

k_means <- kmeans(db,centers = k,nstart = 100)

plot(
  db,
  col = k_means %>% chuck('cluster'),
  main = sprintf('Algoritmo k_medias com %d clusters',k), xlab = 'privileges', ylab = 'learning', pch = 20, cex = 1.2)





k_range <- 2:15


total_dispersion <- numeric(length(k_range))

load(attitude)
attitude
for (k i 1:length(k_range)) {
  k_means <- kmeans(db,centers = k_range[k],nstart = 100)
  
  total_dispersion[k] <- k_means$tot.withinss
}

plot(k_range,total_dispersion,type = 'b',lwd = 2,col = 'blue', yaxt = 'n',xlab = 'k',ylab = 'total dispersion inter cluster')

library(tidyverse)
library(tidylog)

attitude %>% 
  as_tibble() %>% 
  select(rating,complaints) %>% 
  filter(rating < 50)


detach("package:tidylog", unload = TRUE)
detach("package:tidylog")
