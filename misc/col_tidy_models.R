library(ISLR)
library(tidymodels)
library(plotmo)
data("Credit")

Credit

# fit <- leaps::regsubsets(Balance ~.,
#                   data = Credit,
#                   method = "exhaustive",
#                   nvmax = 11)
# resumo <- summary(fit)
# resumo$adjr2
# 
# 
# fit <- leaps::regsubsets(Balance ~.,
#                   data = Credit,
#                   method = "forward",
#                   nvmax = 11)
# resumo <- summary(fit)
# resumo$adjr2



credit_split <- initial_split(Credit %>% select(-ID), prop = 0.8)


credit_recipe <- training(credit_split) %>%
recipe(Balance ~.) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep()

credit_testing <- credit_recipe %>%
  bake(testing(credit_split)) 

credit_training <- credit_recipe %>% juice()

glimpse(credit_training)


glmnet_model <- parsnip::linear_reg(mode = "regression") %>% 
  set_engine("glmnet") %>% 
  fit(Balance ~., data = credit_training)

glmnet_model$fit$beta
plot_glmnet(glmnet_model$fit)
multi_predict(glmnet_model, credit_testing) %>% View()
predict(glmnet_model,credit_testing)

glmnet_model %>%
  predict(credit_testing) %>%
  bind_cols(credit_testing) %>%
  metrics(truth = Balance, estimate = .pred)

cv_testing <- rsample::vfold_cv(credit_training)
matriz_errors <- glmnet_model %>% 
  multi_predict(credit_testing) %>%
  bind_cols(credit_testing) %>% 
  unnest() %>% 
  group_by(penalty) %>% 
  metrics(truth = Balance, estimate = .pred)


results <- matriz_errors %>%
  filter(.metric == "rmse") %>% 
  arrange(penalty)

results %>% 
  ggplot() +
  aes(x = penalty,y = .estimate) +
  geom_line()
