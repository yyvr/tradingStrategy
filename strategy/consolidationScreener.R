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

xx <- ss[ss$Last.Sale > 2, ]

start <- '2020-12-01'
end <- '2022-02-17'
peirod <- paste(start, end)
meta <- NULL

files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
           full.names=TRUE)
#s <- read.csv(paste('/Users/yang/Downloads/invest/usprices/', 'RBCAA', sep=''))

screener <- function(start, end) {
  meta <- NULL
  for ( i in 1:length(files) ) {
    stock <- read.csv(files[i])
    stock <- stock[as.Date(stock$date) <= as.Date(end), ]
    
    if (is.null(stock)) next
    if (nrow(stock) < 250) next
    if (sum(is.na(stock), na.rm = TRUE) != 0) next
    if (any(is.na(stock$close))) next
    head <- nrow(stock) - 60
    tail <- nrow(stock)
    if (stock$close[tail]/stock$close[1] > 1.6) next
    
    # price > ema120
    x <- stock$close[head : tail] < EMA(stock$close, 60)[head : tail]
    if (sum(x, na.rm = TRUE) > 2) {
      next
    }
    
    if (stock$close[tail] < EMA(stock$close, 20)[tail])
      next
    
    # price < 1.01 * ema20
    if (stock$close[tail] >= 1.02 * EMA(stock$close, 20)[tail])
      next
    
    if (stock$close[tail] < EMA(stock$close, 20)[tail])
      next
    
    if (EMA(stock$close, 20)[tail] < EMA(stock$close, 30)[tail])
      next
    
    #if (mean(stock$volume[head : tail]) > 1.1 * mean(stock$volume[1 : head]))
    #  next
    
    if (EMA(stock$close, 20)[tail] > 1.03 * EMA(stock$close, 60)[tail])
      next
    
    if (EMA(stock$close, 120)[tail] < EMA(stock$close, 120)[head])
      next
    
    if (stock$close[tail] < EMA(stock$close, 20)[tail])
      next
    
    if (EMA(stock$close, 60)[tail] < EMA(stock$close, 120)[tail])
      next
    
    if (EMA(stock$close, 120)[tail] < EMA(stock$close, 200)[tail])
      next
    
    if (EMA(stock$close, 60)[head] < EMA(stock$close, 120)[head])
      next
    
    vol <- max(stock$close[head : tail])/min(stock$close[head : tail])
    if (vol > 1.4 || vol < 1.1) {
      next
    }
    
    print(c(stock$symbol[1], i))
    meta <- rbind(meta, c(stock$symbol[1], 
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

meta1 <- screener(start, '2022-01-03')
meta2 <- screener(start, '2022-02-01')

returnTestt <- function(meta, end, duration) {
  result <- NULL
  for (i in 1 : nrow(meta)) {
    filename <- paste('/Users/yang/Downloads/invest/usprices/', meta$stock[i], sep='')
    stock <- read.csv(filename)
    index <- which(as.Date(stock$date) == as.Date(end))
    ret <- stock$close[index + duration]/stock$close[index] - 1
    rr <- c(meta$stock[i], 
            stock$date[index], 
            stock$close[index],
            stock$date[index + duration], 
            stock$close[index + duration],
            round(ret, 3))
    result <- rbind(result, rr)
  }
  
  result <- data.frame(result)
  colnames(result) <- c('stock', 'buyDate', 'buyPrice', 'sellDate', 
                     'sellPrice', 'return')
  result$return <- as.numeric(result$return)
  return(result)
}

result1 <- returnTestt(meta1, '2022-01-03', 5)
postiveReturn <- result1[as.numeric(result1$return) > 0, ]

result2 <- returnTestt(meta2, '2022-02-1', 5)
postiveReturn2 <- result1[as.numeric(result2$return) > 0, ]

meta$`ema20/60` <- as.numeric(meta$`ema20/60`)
meta$marketCap <- as.numeric(meta$marketCap)
#meta <- meta[meta$`ema20/60` < 1.02, ]
#meta$marketCap <- meta$marketCap/1000000
meta$industry <- as.factor(meta$industry)

industries <- levels(ss$Industry)
write.csv(meta, '/Users/yang/Downloads/invest/consolicatedResult.csv')

stock <- tq_get('TSLA',                    
                from = start,
                to = end,
                get = "stock.prices")
head <- nrow(stock) - 100
tail <- nrow(stock)
ema60 <- EMA(stock$close, 60)
result <- summary(lm(ema60[(length(ema60) - 99) : length(ema60)] ~ c(1 : 100)))$coefficients
