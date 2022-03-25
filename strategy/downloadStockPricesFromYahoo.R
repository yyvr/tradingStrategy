library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

all <- read.csv('/Users/yang/Downloads/invest/all.csv')
all <- data.frame(all)
all <- all[all$Market.Cap > 1000000000/2, ]
all <- all[!is.na(all$Symbol), ]
all$Sector <- as.factor(all$Sector)
all$Industry <- as.factor(all$Industry)

tickers <- all$Symbol[(217+443+250+22+394+426+288+357+1147):nrow(all)]

start <- '2020-01-01'
end <- '2022-03-24'
peirod <- paste(start, end)
# length(tickers)

meta <- NULL
for ( i in 1:length(tickers)) {
  if (is.na(tickers[i]))
    next
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