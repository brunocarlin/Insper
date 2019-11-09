library(tidyverse)
library(tidymodels)
library(class)

spam <- read_csv("advanced_predictive_modeling/data/spam.csv")

set.seed(1234)
spam

spam <- spam %>% 
  mutate(Class = Class %>% as.factor())

spam_split <- initial_split(spam,0.7)

spam_training <- spam_split %>% training()
spam_testing <- spam_split %>% testing()

library(tree)


# Tree

ctree <- tree::tree(Class ~ .,data = spam_training)

ctree

y_hat <- predict(ctree,spam_testing, type = "class")

errors <- y_hat == spam_testing$Class

sum(errors)
1- mean(errors)
table(errors)


nearest_neighbor(mode = "classification",neighbors = 1)
narest <- nearest_neighbor(mode = "classification",neighbors = 1)
one_nn <- narest %>%
fit(Class ~ .,data = spam_training)

one_nn %>% 
  predict(spam_testing) %>% 
  bind_cols(spam_testing) %>% 
  metrics(truth = Class,estimate = .pred_class)

# Bagging

forest <- rand_forest(mode = "classification",mtry = ncol(spam_training) -1) %>% 
  fit(Class ~ .,data = spam_training)

forest %>% 
  predict(spam_testing) %>% 
  bind_cols(spam_testing) %>% 
  metrics(truth = Class,estimate = .pred_class)

# Baggign 1nn - fixing needed
x_trn <- spam_training %>% select(-Class) %>% as.matrix()
x_tst <- spam_training %>% select(-Class) %>% as.matrix()

B <- 100
y_hat <- matrix(nrow = x_trn %>% nrow,ncol = B)

pb <- txtProgressBar(min = 1,max = B,style = 3)
for(i in 1:B){
  xs <- sample(1:57,size = 25,replace = FALSE)
  boot <- sample(1:nrow(x_trn),size = nrow(x_trn),replace = TRUE)
  y_hat[,i] <- as.character(knn(x_trn[boot,xs],x_tst[,xs],spam_training$Class[boot],k=1))
  setTxtProgressBar(pb,i)
}
close(pb)

err <- mean(apply(y_hat, 1, function(row) names(sort(table(row),decreasing = TRUE))[1]!= spam_testing$Class))

# Random forest

forest <- rand_forest(mode = "classification") %>% 
  fit(Class ~ .,data = spam_training)

forest %>% 
  predict(spam_testing) %>% 
  bind_cols(spam_testing) %>% 
  metrics(truth = Class,estimate = .pred_class)


