library(tidyverse)

X <- read_csv("advanced_predictive_modeling/data/digits.csv", col_names = F) %>%
  as.matrix()

dim(X)

show_digit <- function(pixels){
  image(matrix(pixels, nrow = 28, byrow = FALSE)[,28:1],
        col = gray((255:0)/255),
        xaxt = "n", yaxt = "n", ann=F)
}

par(mfrow=c(6,6), mar=rep(0,4))

for(i in 1:36){
  show_digit(X[i,])
}

db <- read_csv('advanced_predictive_modeling/data/creditcard.zip')

X <- db %>% 
  select(-Class) %>% 
  scale()

set.seed(1234)

y <- db$Class
idx <- (y == 0) & sample(TRUE:FALSE, size = nrow(X), replace = TRUE)

X_trn <- X[idx,]

X_tst <- X[!idx,]
y_tst <- y[!idx]

pca <- prcomp(X_trn)

r <- 23

proj <- predict(pca, X_tst)

X_hat <- proj[, 1:r] %*% t(pca$rotation[, 1:r])

error <- rowSums((X_tst - X_hat) ^ 2)

worst <- y_tst[order(error,decreasing = TRUE)][1:100]

sum(worst)
