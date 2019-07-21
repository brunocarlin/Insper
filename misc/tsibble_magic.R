devtools::install_github("tidyverts/fasster")

library(fable)
library(fasster)
library(forecast)
library(tsibbledata)
library(tidyverse)
library(tsibble)
df <- tsibble::tourism

df %>% 
  model(ETS(Count))


tourism_aggregated <- tourism %>% 
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))


library(future)
plan(multiprocess)
tourism_fit <- tourism_aggregated %>%
  filter(Quarter < yearquarter("2015 Q1")) %>% 
  model(
    ets_n = ETS(Trips ~ trend("N")),
    ets_a = ETS(Trips ~ trend("A")),
    arima = ARIMA(Trips)
  ) %>% 
  mutate(combn = (ets_n + ets_a + arima)/3)

tourism_fc_reconciled <- tourism_fit %>% 
  reconcile(coherent = min_trace(combn, method = "shrink")) %>%
  forecast(h = "3 years")

tourism_fc_reconciled %>% 
  accuracy(tourism_aggregated) %>% 
  group_by(.model) %>%
  summarise_at(vars(ME:ACF1), median) %>% 
  arrange(MASE)

