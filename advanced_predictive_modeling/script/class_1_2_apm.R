library(tidyverse)
library(tidymodels)

titanic <- read.csv("predictive_models/data/titanic.csv")

titanic %>% str

titanic %>% glimpse()

sum(titanic$Survived/nrow(titanic))

titanic %>%
  group_by(Pclass) %>% 
  mutate(new_age = if_else(Age %>% is.na(),
                           median(Age,na.rm = TRUE),
                           Age)) %>% View()


titanic_c <- titanic %>%
  mutate(Cabin = ifelse(Cabin == "",NA,Cabin),
         Survived = if_else(Survived == 0,
                            "Survive",
                            "Died"))
titanic_recipe <- titanic_c %>% 
  recipe(Survived ~.) %>% 
  step_medianimpute(all_numeric(),-all_outcomes()) %>% 
  step_modeimpute(all_nominal()) %>%
  step_string2factor(all_nominal()) %>% 
  prep()

titanic_preped <- titanic_recipe %>% juice()

set.seed(1,
         kind = "Mersenne-Twister",
         normal.kind = "Inversion")

titanic_split <- titanic_preped %>% initial_split(0.8)
titanic_training <- titanic_split %>% training()
titanic_testing <- titanic_split %>% testing()

# SVM ---------------------------------------------------------------------

svm_model <- svm_poly(mode = "classification") %>% 
  fit(Survived ~ .,data = titanic_training)

svm_model %>% 
  predict(titanic_testing) %>% 
  bind_cols(titanic_testing) %>% 
  metrics(Survived,.pred_class)

svm_model %>% 
  predict(titanic_testing,type = "prob") %>% 
  bind_cols(titanic_testing) %>% 
  yardstick::roc_auc(Survived,.pred_Died)

svm_model %>% 
  predict(titanic_testing,type = "prob") %>% 
  bind_cols(titanic_testing) %>% 
  yardstick::roc_curve(Survived,.pred_Died) %>% 
  autoplot()

class_prob_metrics <- metric_set(average_precision,gain_capture,mn_log_loss,pr_auc,roc_auc)

svm_model %>% 
  predict(titanic_testing,type = "prob") %>% 
  bind_cols(titanic_testing) %>% 
  class_prob_metrics(Survived,.pred_Died)

library(plotly)
p <- svm_model %>% 
  predict(titanic_testing,type = "prob") %>% 
  bind_cols(titanic_testing) %>% 
  pr_curve(Survived,.pred_Died) %>% 
  autoplot()

p %>% ggplotly()

library(probably)
threshold_data <- svm_model %>% 
  predict(titanic_testing,type = "prob") %>% 
  bind_cols(titanic_testing) %>% 
  threshold_perf(Survived,.pred_Died,thresholds = seq(0, 1, by = 0.001)) %>%
  filter(!.metric == "distance")

max_j_index_threshold <- threshold_data %>%
  filter(.metric == "j_index") %>%
  filter(.estimate == max(.estimate)) %>%
  pull(.threshold) %>% 
  median()

p <- threshold_data %>%
  ggplot() + 
  aes(x = .threshold,y =.estimate,color = .metric) +
  geom_vline(xintercept = max_j_index_threshold, alpha = .6, color = "grey30") +
  geom_line() +
  scale_color_viridis_d()

p %>% ggplotly()