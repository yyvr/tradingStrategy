library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

all <- read.csv('/Users/yang/Downloads/invest/all.csv')
all <- data.frame(all)
all <- all[!is.na(all$Symbol), ]
all$Sector <- as.factor(all$Sector)
all$Industry <- as.factor(all$Industry)

tickers <- c('DOX', 'CINF', 'HSIC', 'CTSH')

start <- '2020-01-01'
end <- '2022-04-08'
peirod <- paste(start, end)
meta <- NULL
spy <- tq_get('SPY', from = '2022-01-01', to = end, get = "stock.prices")
for ( i in 1:length(tickers)) {
  if (is.na(tickers[i]))
    next
  print(i)
  tickers[i] <- trimws(tickers[i], whitespace = "[\\h\\v]")
  stock <- tq_get(tickers[i],                    
                  from = start,
                  to = end,
                  get = "stock.prices")
  tail <- nrow(stock)
  coeff <- summary(lm(EMA(stock$close, 60)[(tail - 100) : tail] ~ c(1 : 101)))$coefficients[2]
  
  stock <- stock[stock$date >= as.Date('2022-01-01'), ]
  if (nrow(stock) != nrow(spy)) next
  
  stock$rs <- spy$close[1]/stock$close[1]*stock$close/spy$close - 1
  
  stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
  var(stock$ret*100)
  
  x <- stock$ret > 0
  y <- stock$rs > 0

  index <- which(all$Symbol == tickers[i])
  meta <- rbind(meta, c(tickers[i], 
                        var(stock$ret*100), 
                        all$Market.Cap[index],
                        as.character(all$Industry[index]),
                        coeff,
                        sum(x, na.rm = TRUE),
                        sum(y, na.rm = TRUE)))
}

meta <- data.frame(meta)
colnames(meta) <- c('stock', 'variance', 'marketCap', 'industry', 'coeff', 
                    'dailyReturn', 'relativeStrength')
meta$variance <- as.numeric(meta$variance)
meta$marketCap <- as.numeric(meta$marketCap)
meta$coeff <- as.numeric(meta$coeff)
