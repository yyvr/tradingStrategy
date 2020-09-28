# buy when (macd - signal) turns to positive at next day's close, 
# sell when (macd - signal) starts to diminish

library(TTR)
fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)
results <- NULL

trades <- NULL
for (i in 1:1)#num_stocks)
{
  buys <- NULL
  sells <- NULL
  sellsDate <- NULL
  #if (file_list[i] != 'TYRES.HE.txt') next
  #stock <- read.csv(file_list[i])
  stock <- read.csv('/Users/yang/Downloads/SAMPO-2017-05-01-2020-09-25.csv')
  stock$Date <- as.Date(stock$Date)
  stock <- stock[order(stock$Date), ]
  macd = MACD(stock$Close, nFast=12, nSlow=26, nSig=9, maType=EMA, percent = FALSE)
  rsi <- RSI(stock$Close, 14)
  d <- macd[, 1] - macd[, 2]
  for (j in 35:(nrow(macd) - 1))
  {
    if (d[j] > 0 & macd[j, 1] < 0 & d[j - 1] < 0 & rsi[j] < 70)
    {
      if (stock$Close[j + 1] > 1.1 * stock$Close[j]) next
      buys <- rbind(buys, stock[(j + 1), ])
      found <- 0
      for (k in (j + 2) : nrow(macd))
      {
        if (stock$Low[k] < 0.95 * stock$Close[j + 1])
        {
          sells <- c(sells, 0.95 * stock$Close[j + 1])
          found <- 1
          break
        }
        
        if (stock$High[k] >= 1.15 * stock$Close[j + 1])
        {
          sells <- c(sells, 1.15 * stock$Close[j + 1])
          found <- 1
          break
        }
        
        if (d[k] > 0 & d[k] < d[k - 1])
        {
          sells <- c(sells, stock$Close[k])
          found <- 1
          break
        }
      }
      if (found == 0)
        sells <- c(sells, stock$Close[nrow(stock)])
      sellsDate <- c(sellsDate, as.character(stock$Date[k]))
    }
  }
  ret <- sells/buys$Close - 1
  interval <- as.Date(sellsDate) - as.Date(buys$Date)
  final <- data.frame(file_list[i], buys$Date, buys$Close, sellsDate, sells, ret, interval)
  trades <- rbind(trades, final)
}

macd <- cbind(macd, macd[, 1] - macd[, 2])
macd <- cbind(as.character(stock$Date), macd)

meta <- NULL
for (i in 1:num_stocks)
{
  tmp <- trades[trades$file_list.i. == file_list[i], ]
  if (sum(tmp$ret) > 0.1)
    meta <- rbind(meta, c(file_list[i], sum(tmp$ret)))
}