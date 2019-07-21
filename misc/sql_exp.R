library(odbc)
library(tidyverse)
library(tidylog)
con <- DBI::dbConnect(RSQLite::SQLite(), path = ":dbname:")
copy_to(dest = con,df = nycflights13::flights,name = "flights") %>% show_query()
con %>% db_list_tables()
tidylog::
tbl1 <- tbl(con,"flights")
tbl1 %>%tidylog::filter(month ==1) %>% collect()
library(nycflights13)
flights <- nycflights13::flights 
flights %>% filter(month == 1)
tidylog(flights)
