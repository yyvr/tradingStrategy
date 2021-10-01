library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

ss <- read.csv('/Users/yang/Downloads/invest/nasdaq_screener_1627328838671.csv')
xx <- ss[ss$IPO.Year < 2019 & (!is.na(ss$IPO.Year)), ]
tickers <- xx$Symbol[30:100]

start <- '2020-06-01'
end <- '2021-09-30'
peirod <- paste(start, end)
final <- NULL
meta <- NULL
allresults <- NULL

screener <- function(tickers, start, end) {
  for ( i in 1:length(tickers) ) {
    stock <- tq_get(tickers[i],                    
                    from = start,
                    to = end,
                    get = "stock.prices")
    if (nrow(stock) < 120) next
    if (any(is.na(stock$close))) next
    head <- nrow(stock) - 100
    tail <- nrow(stock)
    
    x <- stock$close[head : tail] < EMA(stock$close, 120)[head : tail]
    if (sum(x, na.rm = TRUE) != 0)
      next
    
    if (stock$close[tail] >= 1.1 * EMA(stock$close, 20)[tail])
      next
    
    if (mean(stock$volume[head : tail]) > 1.1 * mean(stock$volume[1 : head]))
      next
    
    if (EMA(stock$close, 120)[tail] < EMA(stock$close, 120)[head])
      next
    
    if (stock$close[tail] < EMA(stock$close, 30)[tail])
      next
    
    if (EMA(stock$close, 60)[tail] < EMA(stock$close, 120)[tail])
      next
    
    if (EMA(stock$close, 60)[head] < EMA(stock$close, 120)[head])
      next
    
    vol <- max(stock$close[head : tail])/min(stock$close[head : tail])
    if (vol > 1.25) {
      next
    }
    meta <- rbind(meta, c(tickers[i], vol, mean(stock$volume[head : tail])/mean(stock$volume[1 : head])))
  }
  print(meta)
  meta <- data.frame(meta)
  colnames(meta) <- c('stock', 'priceChange', 'volumnChange')
  meta$priceChange <- as.numeric(meta$priceChange)
  meta[order(meta$priceChange), ]
  return(meta[1:5, ])
}

meta <- screener(tickers, start, end)


