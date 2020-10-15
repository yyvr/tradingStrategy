# buy when (macd - signal) turns to positive at next day's close, 
# sell when (macd - signal) starts to diminish

library(TTR)
fp <- "/Users/yang/Downloads/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)
results <- NULL

trades <- NULL
for (i in 1:num_stocks)
{
  buys <- NULL
  sells <- NULL
  sellsDate <- NULL
  #if (file_list[i] != 'NOKIA-2015-01-01-2020-09-28.csv') next
  stock <- read.csv(file_list[i])
  #stock <- read.csv('/Users/yang/Downloads/SAMPO-2017-05-01-2020-09-25.csv')
  stock$Date <- as.Date(stock$Date)
  stock <- stock[order(stock$Date), ]
  macd = MACD(stock$Close, nFast=12, nSlow=26, nSig=9, maType=EMA, percent = FALSE)
  rsi <- RSI(stock$Close, 14)
  d <- macd[, 1] - macd[, 2]
  reasons <- NULL
  for (j in 35:(nrow(macd) - 2))
  {
    if (d[j] > 0 & macd[j, 1] < 0 & d[j - 1] < 0 & rsi[j] < 65)
    {
      if (stock$Close[j + 1] > 1.1 * stock$Close[j]) next
      buys <- rbind(buys, stock[(j + 1), ])
      stoploss <- 0.95 * stock$Close[j + 1]
      found <- 0
      for (k in (j + 2) : nrow(macd))
      {
        # stop loss at 5%
        if (stock$Close[k] < stoploss & stock$High[k] > stoploss)
        {
          sells <- c(sells, stoploss)
          found <- 1
          reasons <- c(reasons, 'Stop loss')
          break
        }

        # stop loss at 5%
        if (stock$Close[k] < stoploss & stock$High[k] < stoploss)
        {
          sells <- c(sells, stock$Close[k])
          found <- 1
          reasons <- c(reasons, 'Stop loss at close')
          break
        }
        
        # lock in profit if rise over 15% 
        if (stock$High[k] >= 1.15 * stock$Close[j + 1])
        {
          sells <- c(sells, 1.15 * stock$Close[j + 1])
          found <- 1
          reasons <- c(reasons, 'Lock in profit at 15%')
          break
        }
        
        # sell when upward momentum weakens
        if (d[k] > 0 & d[k] < (0.95 * d[k - 1]))
        {
          sells <- c(sells, stock$Close[k])
          found <- 1
          reasons <- c(reasons, 'MACD weakens')
          break
        }
      }
      if (found == 0)
      {
        sells <- c(sells, stock$Close[nrow(stock)])
        reasons <- c(reasons, 'End of sample')
      }
      sellsDate <- c(sellsDate, as.character(stock$Date[k]))
    }
  }
  ret <- sells/buys$Close - 1
  interval <- as.Date(sellsDate) - as.Date(buys$Date)
  final <- data.frame(file_list[i], buys$Date, buys$Close, sellsDate, sells, 
                      ret, interval, reasons)
  if (sum(final$ret) > 0.3)
    trades <- rbind(trades, final)
}

macd <- cbind(macd, macd[, 1] - macd[, 2])
macd <- cbind(as.character(stock$Date), macd)

meta <- NULL
for (i in 1:num_stocks)
{
  tmp <- trades[trades$file_list.i. == file_list[i], ]
  if (sum(tmp$ret) > 0.3)
    meta <- rbind(meta, c(file_list[i], sum(tmp$ret)))
}
trades <- trades[order(trades$Date), ]
#write.csv2(trades, '/Users/yang/Downloads/trades.csv', sep = ';')
