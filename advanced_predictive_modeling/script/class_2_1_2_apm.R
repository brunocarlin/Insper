# Neural Network
library(tidyverse)
library(tidymodels)
library(class)

spam <- read_csv("advanced_predictive_modeling/data/spam.csv")

set.seed(1234)


spam <- spam %>% 
  mutate(Class = Class %>% as.factor())

spam_split <- initial_split(spam,0.7)

spam_training <- spam_split %>% training()
spam_testing <- spam_split %>% testing()

spam_recipe <- spam_training %>% 
recipe(Class ~.) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep()

spam_training_df <- spam_recipe %>% juice()
spam_testing_df <- spam_recipe %>% bake(spam_testing)



x_trn <- as.matrix(spam_training_df[,1:57])
y_trn <- to_categorical(spam_training_df$Class %>% as.integer() -1,num_classes = 2)

x_tst <- as.matrix(spam_testing_df[,1:57])
y_tst <- to_categorical(spam_testing_df$Class %>% as.integer() -1,2)

library(keras)
use_session_with_seed(1234,
                      disable_parallel_cpu = TRUE,
                      disable_gpu = TRUE)

net <- keras_model_sequential()

net <- layer_dense(net,units = 64,activation = 'relu',input_shape = ncol(x_trn))
net <- layer_dropout(net,rate = 0.2)
net <- layer_dense(net,units = 32,activation = 'relu')
net <- layer_dropout(net,rate = 0.2)
net <- layer_dense(net,units = 2,activation = 'softmax')

summary(net)

net <- compile(net,
               loss = 'categorical_crossentropy',
               optimizer = 'adam',
               metrics = 'accuracy')

historyy <- fit(net, x_trn, y_trn, epochs = 30, batch_size = 128,validation_split = 0.2)

y_hat_net <- predict_classes(net,x_tst)


mean(y_hat_net - y_tst[,2])



# Regression ------------------------------------------------------------------------------------------------------

library(MASS)
library(tidymodels)

boston_split <- Boston %>% initial_split(0.7)

boston_training <- boston_split %>% training()
boston_testing <- boston_split %>% testing()

model_forest <- rand_forest(mode = 'regression') %>% 
  fit(medv ~ .,data  = boston_training)

model_forest %>% 
  predict(boston_testing) %>% 
  bind_cols(boston_testing %>% select(medv)) %>% 
  metrics(.pred,medv)



boston_split <- Boston %>% initial_split(0.7)


boston_training <- boston_split %>% training()
boston_testing <- boston_split %>% testing()

boston_recipe <- boston_training %>% 
  recipe(medv ~ .) %>%
  step_normalize(all_numeric(),-all_outcomes()) %>% 
  prep()
  

boston_training_prep <- boston_recipe %>% juice()
boston_testing_prep <- boston_recipe %>% bake(boston_testing)

boston_trn_x <- boston_training_prep %>% select(-medv) %>% as.matrix()
boston_trn_y <- boston_training_prep %>% select(medv) %>% as.matrix()
use_session_with_seed(1234,
                      disable_parallel_cpu = TRUE,
                      disable_gpu = TRUE)
boston_tst_x <- boston_testing_prep %>% select(-medv) %>% as.matrix()
boston_tst_y <- boston_testing_prep %>% select(medv) %>% as.matrix()

library(keras)
use_session_with_seed(1234,
                      disable_parallel_cpu = TRUE,
                      disable_gpu = TRUE)
net <- keras_model_sequential()

net <- layer_dense(net,units = 32,activation = 'relu',input_shape = ncol(boston_trn_x))
net <- layer_dense(net,units = 16,activation = 'relu')
net <- layer_dense(net,units = 1)

net <- compile(net,loss = 'mse',optimizer = 'adam',metrics = 'mse')

summary(net)

history <- fit(net,boston_trn_x,boston_trn_y,batch_size = 16,epochs = 50,validation_split = 0.2)

y_hat_net <- predict(net,boston_tst_x)

(RMSE_net <- sqrt(mean((y_hat_net - boston_tst_y) ^2)))
