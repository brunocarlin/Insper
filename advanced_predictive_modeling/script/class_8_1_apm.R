library(tidyverse)
library(tidymodels)

rm(list = ls())

options(digits = 2)

USArrests


X <- scale(USArrests[, c('Murder','Assault')],
           center = TRUE, scale = FALSE)

colMeans(X)

X %>% plot()


(X)
pca <- prcomp

pca$x


X <- scale(USArrests,
           center = TRUE, scale = TRUE)

pca <- prcomp(X)

pca$rotation <- -pca$rotation
pca$x <- -pca$x

(Phi <- pca$rotation)

Z <- pca$x

Z %>% head

biplot(pca, scale = 0,cex = 0.75, xlab = 'PC1: criminality', ylab = 'PC2: Urbanization')


install.packages('factoextra')

library(factoextra)

fviz_pca_biplot(pca,repel = TRUE, xlab = 'PC1: criminality',ylab = 'PC2: Urbanization',axes = c(1,2), gradient.cols = c("white","red"))


