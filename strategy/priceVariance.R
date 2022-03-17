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
files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)

screener <- function(start, end) {
  meta <- NULL
  for ( i in 1:length(files) ) {
    stock <- read.csv(files[i])
    stock <- stock[as.Date(stock$date) <= as.Date(end), ]
    stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
    
    if (is.null(stock)) next
    if (nrow(stock) < 250) next
    if (sum(is.na(stock), na.rm = TRUE) != 0) next
    if (any(is.na(stock$close))) next
    head <- nrow(stock) - 50
    tail <- nrow(stock)
    if (stock$close[tail]/stock$close[1] > 1.6) next
    
    # price > ema60
    x <- stock$close[head : tail] < EMA(stock$close, 150)[head : tail]
    if (sum(x, na.rm = TRUE) > 0) {
      next
    }
    
    # price < 1.01 * ema20
    if (stock$close[tail] >= 1.03 * EMA(stock$close, 20)[tail])
      next
    
    index <- which(all$Symbol == stock$symbol[1])
    meta <- rbind(meta, c(stock$symbol[1], 
                          var(stock$ret[(tail - 100):tail] * 100), 
                          all$Market.Cap[index],
                          as.character(all$Industry[index])))
  }
  meta <- data.frame(meta)
  colnames(meta) <- c('stock', 'variance', 'MarketCap', 'industry')
  meta$MarketCap <- as.numeric(meta$MarketCap)/1000000000
  meta$variance <- as.numeric(meta$variance)
  meta$industry <- as.factor(meta$industry)
  return (meta)
}

meta1 <- screener(start, '2022-03-17')
meta2 <- screener(start, '2022-03-10')
result1 <- meta1 %>% group_by(industry) %>% summarise('2022/03/17' = length(industry))
result2 <- meta2 %>% group_by(industry) %>% summarise('2022/03/10' = length(industry))
data_merge <- merge(result2, result1, by = 'industry')

