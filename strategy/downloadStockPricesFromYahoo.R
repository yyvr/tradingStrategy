library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

ss <- read.csv('/Users/yang/Downloads/invest/nyse.csv')
ss1 <- read.csv('/Users/yang/Downloads/invest/nasdaq.csv')
ss2 <- read.csv('/Users/yang/Downloads/invest/amex.csv')
all <- rbind(ss, ss1, ss2)
all$Sector <- as.factor(all$Sector)
all$Industry <- as.factor(all$Industry)

tickers <- all$Symbol[(599+595+91+788):2072]

start <- '2020-01-01'
end <- '2022-03-17'
peirod <- paste(start, end)
# length(tickers)

stock <- tq_get('CWK', from = start, to = end, get = "stock.prices")

meta <- NULL
for ( i in 1:length(tickers)) {
  print(i)
  tickers[i] <- trimws(tickers[i], whitespace = "[\\h\\v]")
  stock <- tq_get(tickers[i],                    
                  from = start,
                  to = end,
                  get = "stock.prices")
  stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
  if (is.null(stock)) next
  if (nrow(stock) < 250) next
  if (sum(is.na(stock), na.rm = TRUE) != 0) next
  if (any(is.na(stock$close))) next
  write.csv(stock, paste('/Users/yang/Downloads/invest/usprices/', tickers[i], sep=''))
}