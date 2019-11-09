library(tidyverse)
library(ISLR)

head(Credit)

Credit <- Credit %>% select(-ID)

theme_set(theme_bw())

Credit %>% 
  ggplot() +
  aes(x = Income,y = Balance,color = Student,size = Age) +
  geom_point() +
  geom_smooth(method = 'lm',formula = y ~ poly(x,2))

library(ggforce)

library(tidyverse)
Credit %>% 
  ggplot() +
  geom_autodensity() +
  aes(x = Ethnicity ,y = Balance) + facet_matrix(vars(everything()),layer.diag = 2)

library(ggcorrplot)

Credit %>%
  select_if(is.numeric) %>% 
  ggplot() +
  aes(x = .panel_x,y = .panel_y) +
  geom_point() +
  gg() +
  geom_() +
  facet_matrix(vars(everything()),layer.diag = 2,layer.upper = 3)
  

Credit %>% 
  GGally::ggpairs()


Credit %>% 
  ggplot() +
  aes(Balance) +
  geom_histogram()



df <- read_csv('advanced_predictive_modeling/data/Exemplo1.csv')

df %>% 
  GGally::ggpairs()

fit1 <- lm(y ~ .,data = df)

summary(fit1)

erros <- fit1 %>% pluck(fitted.values) - df %>% pull(y)
erros_2 <- erros ^ 2
mean1 <- mean(erros_2)


df %>% 
  ggplot() +
  aes(x1,y,color = x2) +
  geom_point() +
  geom_smooth()


fit1 <- lm(y ~ .*.,data = df)

summary(fit1)

erros <- fit1 %>% pluck(fitted.values) - df %>% pull(y)
erros_2 <- erros ^ 2
mean1 <- mean(erros_2)



fit2 <- lm(y ~ log(x1)*x2 ,data = df)

summary(fit2)

erros <- fit2 %>% pluck(fitted.values) - df %>% pull(y)
erros_2 <- erros ^ 2
mean1 <- mean(erros_2)


x <- rgamma(100,1,1)

hist(x)

library(bestNormalize)
x_transf <- boxcox(x)
hist(x_transf$x.t)


housing_full <- read_csv('advanced_predictive_modeling/data/housing.csv')

library(tidymodels)

housing_split <- housing_full %>% 
  filter(ocean_proximity != 'ISLAND',total_bedrooms %>% is.na() %>% `!`) %>% 
  initial_split(prop = 0.75)
  
testing <- housing_split %>% testing()
training <- housing_split %>% training()  

cv_groups <- sample(1:10,nrow(training),replace =TRUE)

cv_lm <- vector('numeric',10)

rmse_2 <- function(x,y){
  erro <- x-y
  erros_2 <- erro ^ 2
  rmse <- erros_2 %>% mean()
  return(rmse)
}

library(base)
for (i in 1:10) {
  data_training <- training[cv_groups!=i,]
  data_validation1 <- training[cv_groups==i,]
  true_values <- training$median_house_value[cv_groups == i]
  fit_lm1 <- rand_forest(mode = "regression",trees = 100) %>% fit(median_house_value ~ .,data = data_training)
  prediction_lm1 <- predict(fit_lm1,new_data = data_validation1)
  cv_lm[i] <- prediction_lm1 %>% 
    bind_cols(true_values %>% as.tibble()) %>% 
  metrics(.pred,value) %>%
    filter(.metric == 'rmse') %>% 
    pull(.estimate)
}


cv_lm %>% summary()

library(h2o)


h2o.init(nthreads = 2,max_mem_size = "1g")

simple_train_hex <-  as.h2o(training)
simple_test_hex = as.h2o(testing)

# simple_y_hex <- simple_train_hex %>% select(median_house_value) %>% pull %>% as.numeric()
# simple_x_hex <- simple_train_hex %>% select(-median_house_value)


aml <- h2o.automl(y = "median_house_value",
                  training_frame = simple_train_hex,
                  max_runtime_secs = 200,
                  seed = 1)

aml
summary(aml)

model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]

m <- h2o.getModel(model_ids[model_ids %>% str_detect("GBM_4_AutoML")])

result_predictions <- predict(m,simple_test_hex)


result_predictions %>% 
  as_tibble() %>% 
  bind_cols(testing) %>% 
  metrics(truth = median_house_value,estimate = predict)

h2o.shutdown()
