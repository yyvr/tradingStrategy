library(quantmod)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)
library(tidyquant)

all <- read.csv('/Users/yang/Downloads/invest/tickers/nasdaq_screener_1667650603297.csv')
all <- data.frame(all)
all <- all[!is.na(all$Symbol), ]
all$Sector <- as.factor(all$Sector)
all$Industry <- as.factor(all$Industry)
all <- all[all$Market.Cap > 1000000000, ]
tickers <- all$Symbol[(1):nrow(all)]
start <- '2020-01-01'
end <- as.character(Sys.Date())
peirod <- paste(start, end)
meta <- NULL
for ( i in 1:length(tickers)) {
  if (is.na(tickers[i]))
    next
  
  tickers[i] <- trimws(tickers[i], whitespace = "[\\h\\v]")
  stock <- tq_get(tickers[i],                    
                  from = start,
                  to = end,
                  get = "stock.prices")
  if (is.null(stock)) next
  #if (nrow(stock) < 250) next
  if (sum(is.na(stock), na.rm = TRUE) != 0) next
  if (any(is.na(stock$close))) next
  print(c(i, tickers[i]))
  write.csv(stock, paste('/Users/yang/Downloads/invest/usprices/', tickers[i], sep=''))
}

#symbols <- stockSymbols(exchange = c("AMEX", "NASDAQ", "NYSE"), 
#                        sort.by = c("Exchange", "Symbol"),
#                        quiet = FALSE)

#nyse <- symbols[symbols$Exchange == 'NYSE', ]