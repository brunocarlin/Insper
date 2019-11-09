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

model_forest <- parsnip::rand_forest(mode = "classification") %>% 
  fit(Class ~., data = spam_training)

pr_rf <- predict(model_forest,spam_training, type = "prob") %>%
  pull(.pred_Spam) 


model_boost <- parsnip::boost_tree(mode = "classification") %>% 
  set_engine("xgboost") %>% 
  fit(Class ~., data = spam_training)

pr_boost <- predict(model_boost,spam_training, type = "prob") %>%
  pull(.pred_Spam) 

# library(fastAdaboost)
# 
# boost <- adaboost(Class ~.,data = spam_training,nIter = 50)
# 
# pred <- predict(boost,spam_testing)
# 

# Stacking 

stack_trn <- tibble(rf = pr_rf,boost =pr_boost,Class = spam_training %>% pull(Class))

pr_forest_tst <- predict(model_forest,spam_testing, type = "prob") %>%
  pull(.pred_Spam)

pr_boost_tst <- predict(model_boost,spam_testing, type = "prob") %>%
  pull(.pred_Spam)
  

stack_tst <- tibble(rf = pr_forest_tst,boost = pr_boost_tst,Class = spam_testing %>% pull(Class))

model_tree <- parsnip::logistic_reg(mode = 'classification') %>% 
  fit(Class ~.,data = stack_trn)

pr_stack <- predict(model_tree,stack_tst,type = 'prob')

y_hat_stack <- factor(ifelse(pr_stack$.pred_Spam > 0.54,"Spam","Ham"))

mean(y_hat_stack != stack_tst$Class)


boost2 <- factor(ifelse(pr_boost_tst > .38,'Spam','Ham'))

mean(boost2 != stack_tst$Class)
