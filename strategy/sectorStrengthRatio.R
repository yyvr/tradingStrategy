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
files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)

screener <- function(start, end) {
  meta <- NULL
  for ( i in 1:length(files) ) {
    stock <- read.csv(files[i])
    stock <- stock[as.Date(stock$date) <= as.Date(end), ]
    stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
    
    if (is.null(stock)) next
    if (sum(is.na(stock), na.rm = TRUE) != 0) next
    if (any(is.na(stock$close))) next
    head <- nrow(stock) - 50
    tail <- nrow(stock)
    
    # price > ema150
    x <- stock$close[head : tail] < EMA(stock$close, 150)[head : tail]
    if (sum(x, na.rm = TRUE) > 0) {
      next
    }
    index <- which(all$Symbol == stock$symbol[1])
    meta <- rbind(meta, c(stock$symbol[1], 
                          var(stock$ret[(tail - 100):tail] * 100), 
                          all$Market.Cap[index],
                          as.character(all$Industry[index])))
  }
  meta <- data.frame(meta)
  colnames(meta) <- c('stock', 'variance', 'MarketCap', 'Industry')
  meta$MarketCap <- as.numeric(meta$MarketCap)/1000000000
  meta$variance <- as.numeric(meta$variance)
  meta$Industry <- as.factor(meta$Industry)
  return (meta)
}

meta1 <- screener('2020-12-01', '2022-03-24')
allsectors <- all %>% group_by(Industry) %>% summarise('all' = length(Industry))
strongsectors <- meta1 %>% group_by(Industry) %>% summarise('strong' = length(Industry))
data_merge <- merge(allsectors, strongsectors, by = 'Industry')
data_merge$ratio <- data_merge$strong/data_merge$all
