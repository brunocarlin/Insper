
consumer_key <- 'XMkRYlmnOGKUc6nFX5l3ecWgR' 
consumer_secret <- 'RCyQu3qvsL1qqRB3X0mBNx8opJSuniRvC8cVpEwjverlTnzfcm' 
access_token <- '228553958-ZGpX9zVunGlt0OLcrkLvZrSwlJr9sdMREiLxBvsV'
access_secret <- 'Q2OqgNC5edORaJlrzBSoOh2XdbStWrNiAIwEm6vvmVtFe'

library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(Matrix)
library(topic)

###

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

x <- closestTrendLocations(lat = -23.53,long = -46.67)

trends <- getTrends(woeid = x$woeid)

trends$name[1:10]

results <- searchTwitter('Flamengo',n = 1000,lang = 'pt')

tweets <- twListToDF(results)

dim(tweets)
library(tidyverse)
glimpse(tweets)

txt <- sapply(tweets$text,function(row) iconv(row,'UTF-8','ASCII//TRANSLIT',sub = ''))

corpus <- Corpus(VectorSource(txt))

corpus_clean <- corpus %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removeWords,c('https','rt','via','flamengo')) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords,iconv(stopwords('portuguese'),from = 'UTF-8',to = 'ASCII//TRANSLIT')) %>% 
  tm_map(stemDocument,language = 'portuguese')

wordcloud(corpus_clean,max.words = 40,random.order = FALSE,rot.per = 0,colors = brewer.pal(8,'Dark2'))

dtm <- DocumentTermMatrix(corpus_clean)

findFreqTerms(dtm,lowfreq = 40)

findAssocs(dtm,terms = 'fic',corlimit = 0.3)


# Market Basket Analysis ------------------------------------------------------------------------------------------

library(arules)

options(digits = 3)

data(Groceries)

Groceries
Groceries %>% summary()

rules <- apriori(Groceries,parameter = list(sup = 0.001,
                                            conf = 0.8))

inspect(head(rules,5,by = 'lift'))
inspect(head(rules,5,by = c('lift','confidence')))

rules.sub <- subset(rules, subset = rhs %in% "bottled beer" & lift > 2)
inspect(rules.sub)

