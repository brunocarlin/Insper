library(BatchGetSymbols)
data = BatchGetSymbols(c('PETR4.SA','VALE3.SA','^BVSP'),
                       first.date = '2007-01-01',
                       last.date = Sys.Date(),
                       bench.ticker = "^BVSP", 
                       type.return = "log",
                       freq.data = "daily",
                       do.complete.data = TRUE,
                       do.fill.missing.prices = TRUE)

library(ggplot2)
library(gridExtra)
df.tickers <- data$df.tickers %>%
  filter(volume!=0)
sub <- df.tickers %>% subset(ticker!='^BVSP')
ibov <- df.tickers %>% subset(ticker=='^BVSP')

p1 <- ggplot(sub, aes(x=ref.date, y=price.close, group=ticker)) +
  geom_line(aes(color=ticker))+
  geom_point(aes(color=ticker))+
  theme(legend.position="top")+
  xlab('Data')+ylab('Reais')
p2 <- ggplot(ibov, aes(x=ref.date, y=price.close)) +
  geom_line()+
  geom_point()+
  xlab('Data')+ylab('Pontos - IBOVESPA')
grid.arrange(p1, p2, nrow = 2)

library(xts)
library(dplyr)
library(lubridate)
petro <- df.tickers %>%
  subset(ticker=='PETR4.SA') %>%
  select(ref.date,ret.closing.prices)
ret <- xts(petro[,-1],order.by = ymd(petro$ref.date))[-1,]
ret2 = ret**2
plot(ret2)


par(mfrow=c(1,2))
h <- hist(ret, breaks=20, col="red", xlab="", 
          main="Histogram") 
xfit <- seq(min(ret),max(ret),length=40) 
yfit <- dnorm(xfit,mean=mean(ret),sd=sd(ret)) 
yfit <- yfit*diff(h$mids[1:2])*length(ret) 
lines(xfit, yfit, col="blue", lwd=2)

qqnorm(ret, pch = 1, frame = FALSE)
qqline(ret, col = "steelblue", lwd = 2)

library(moments)
skewness(ret)
kurtosis(ret)
shapiro.test(as.vector(ret))

n <- length(ret)
for(i in 1:floor(log(n))) # n = log(T) practical rule
  print(Box.test(ret, lag = i, type = "Ljung-Box"))

library(FinTS)
ArchTest(ret2,lags=12)


library(fGarch)
m1=garchFit(~1+garch(12,0),data=ret,trace=F)
m1@fit$matcoef

par(mfrow=c(2,1))
plot(ret,ylab="retornos",main='')
sigma <- xts(cbind(m1@sigma.t,m2@sigma.t),order.by = index(ret))
colnames(sigma) <- c('erros normais','erros t')
plot(sigma,auto.legend=T,legend.loc = "top",main='')


ibov <- df.tickers %>%
  subset(ticker=='^BVSP') %>%
  select(ref.date,ret.closing.prices,price.close)



