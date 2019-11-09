library(readr)
library(tidyverse)
sales_data_2018 <- read_delim("~/insper-bigdata/data/sales_data_2018.zip", 
                              "|", escape_double = FALSE, col_names = FALSE, 
                              trim_ws = TRUE,n_max = 1000)

names(sales_data_2018) <- c('product_id','store_id','date','quantity','price')

test_remover <- function(col_text){
  col_text %>% 
    str_remove("\\[") %>% 
    str_remove("\\]") %>%
    str_remove_all(" ") %>% 
    str_remove_all("'") %>%
    str_remove_all("'")
}
  
sales_data_2018 %>% 
  mutate_at(vars(date,quantity,price),test_remover) %>% 
  separate_rows(date,quantity,price,sep = ",",convert = TRUE) %>%
  mutate(individual_price = price/quantity) %>% 
  arrange(-individual_price)
         