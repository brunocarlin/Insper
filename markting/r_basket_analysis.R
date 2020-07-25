
## carregar pacotes	

library(tidyverse)
library(readxl)
library(lubridate)
library(arules)
library(arulesViz)
library(readxl)

## carregar dados	

retail <- read_excel("markting/data/online_retail.xlsx")

## processar e explorar	

retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail <- retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
# retirar os pedidos cancelados
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

## Que horas os clientes compram?	

retail$Time <- as.factor(retail$Time)
a <- hms(as.character(retail$Time))
retail$Time = hour(a)
retail %>% 
  ggplot(aes(x=Time)) + 
  geom_histogram(stat="count",fill="indianred")

## Qual a quantidade de produtos os clientes compram por transa??o?	

detach("package:plyr", unload=TRUE)
retail %>% 
  group_by(InvoiceNo) %>% 
  summarize(n_items = mean(Quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

## Quais s?o os best sellers? (10)	

tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tmp %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

## transformar os dados em colunas (long - wide)	

retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
library(dplyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

## itemList <- ddply(retail,c("CustomerID","Date"), 
##                  function(df1)paste(df1$Description, 
##                                     collapse = "|"))
## existe problema com nome das vars - precisa eliminar as virgulas dos descritores

# remover campos desnecess?rios
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

## entender as transa??es	

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',')
tr
summary(tr)

## freq de compra do intem

itemFrequencyPlot(tr, topN=20, type='absolute')

## regras (support, confidence, lift)	

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

## das 87110 regras quais sao as primeiras (ranqueada por confidence)
inspect(rules[1:10])

topRules <- rules[1:10]
plot(topRules)
plot(topRules, method="graph")
plot(topRules, method = "grouped")


# reorganizar base para PCA

library(tidyr)

invsum <- aggregate(retail$Quantity, by=list(Category= Retail$StockCode), FUN=sum)


invsum <- retail %>% 
  group_by(InvoiceNo,Description) %>% 
  summarize(soma = sum(Quantity),.groups = "drop")


result <- invsum %>% 
  pivot_wider(names_from = Description,values_from = soma,values_fill = 0)



library(dplyr)
tranprod <- group_by(retail, InvoiceNo , StockCode)
tranprod <- summarise(tranprod, sumquant = sum(Quantity))
tranprod 
