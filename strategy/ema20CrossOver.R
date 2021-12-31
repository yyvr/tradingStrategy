library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

ss <- read.csv('/Users/yang/Downloads/invest/nyse.csv')
ss1 <- read.csv('/Users/yang/Downloads/invest/nasdaq.csv')
ss2 <- read.csv('/Users/yang/Downloads/invest/amex.csv')
ss <- rbind(ss, ss1, ss2)
ss$Sector <- as.factor(ss$Sector)
ss$Industry <- as.factor(ss$Industry)

xx <- ss[ss$IPO.Year < 2019 & (!is.na(ss$IPO.Year)) & 
           ss$Last.Sale > 2, ]
tickers <- xx$Symbol[1000:2127]

start <- '2020-10-01'
end <- '2021-12-21'
peirod <- paste(start, end)
final <- NULL
meta <- NULL
allresults <- NULL

screener <- function(tickers, start, end) {
  for ( i in 1:length(tickers) ) {
    tickers[i] <- trimws(tickers[i], whitespace = "[\\h\\v]")
    if (tickers[i] == 'SPKE' || tickers[i] == 'AMR') next
    #if (tickers[i] != 'AIF') next
    stock <- tq_get(tickers[i],                    
                    from = start,
                    to = end,
                    get = "stock.prices")
    if (is.null(stock)) next
    if (nrow(stock) < 180) next
    if (sum(is.na(stock), na.rm = TRUE) != 0) next
    
    if (any(is.na(stock$close))) next
    head <- nrow(stock) - 100
    tail <- nrow(stock)
    
    # price > ema120
    x <- stock$close[head : tail] < EMA(stock$close, 120)[head : tail]
    if (sum(x, na.rm = TRUE) != 0)
      next
    
    # price < 1.01 * ema20
    if (stock$close[tail] >= 1.05 * EMA(stock$close, 20)[tail])
      next
    
    #if (mean(stock$volume[head : tail]) > 1.1 * mean(stock$volume[1 : head]))
    #  next
    
    vol <- max(stock$close[head : tail])/min(stock$close[head : tail])
    
    if (EMA(stock$close, 120)[tail] < EMA(stock$close, 120)[head])
      next
    
    if ((EMA(stock$close, 8)[tail] > EMA(stock$close, 21)[tail])) {
      if ((EMA(stock$close, 8)[tail - 2] < EMA(stock$close, 21)[tail - 2]) ||
          (EMA(stock$close, 8)[tail - 3] < EMA(stock$close, 21)[tail - 3]) ||
          (EMA(stock$close, 8)[tail - 1] < EMA(stock$close, 21)[tail - 1]))
      print(tickers[i])
    }
    
    meta <- rbind(meta, c(tickers[i], 
                          vol, 
                          mean(stock$volume[head : tail])/mean(stock$volume[1 : head]),
                          EMA(stock$close, 20)[tail]/EMA(stock$close, 60)[tail],
                          stock$close[tail]/EMA(stock$close, 20)[tail],
                          as.character(xx$Industry[i]),
                          as.character(xx$Sector[i]),
                          xx$Market.Cap[i],
                          stock$close[tail]/stock$close[1]))
  }
  meta <- data.frame(meta)
  colnames(meta) <- c('stock', 'priceChange', 'volumnChange', 'ema20/60', 
                      'close/ema20', 'industry', 'sector', 'marketCap', 'totalReturn')
  meta$priceChange <- as.numeric(meta$priceChange)
  meta[order(meta$priceChange), ]
  return (meta)
}

meta <- screener(tickers, start, end)
meta$`ema20/60` <- as.numeric(meta$`ema20/60`)
meta$marketCap <- as.numeric(meta$marketCap)
#meta <- meta[meta$`ema20/60` < 1.02, ]
#meta$marketCap <- meta$marketCap/1000000
meta$industry <- as.factor(meta$industry)

industries <- levels(ss$Industry)
write.csv(meta, '/Users/yang/Downloads/invest/consolicatedResult.csv')

xxx <- tq_get('TSLA', get='key.ratios')

appl_key_ratios <- tq_get("AAPL", get = "financials")
