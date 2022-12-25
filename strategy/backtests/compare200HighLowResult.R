high <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/Buy200DaysHigh.csv')
low <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/Buy200DaysLow.csv')
positiveRates <- NULL
positiveRates <- rbind(positiveRates, c(nrow(high[high$ret60 > 0, ])/nrow(high),
                                        nrow(high[high$ret90 > 0, ])/nrow(high),
                                        nrow(high[high$ret120 > 0, ])/nrow(high),
                                        nrow(high[high$ret200 > 0, ])/nrow(high)))
positiveRates <- rbind(positiveRates, c(nrow(low[low$ret60 > 0, ])/nrow(low),
                                        nrow(low[low$ret90 > 0, ])/nrow(low),
                                        nrow(low[low$ret120 > 0, ])/nrow(low),
                                        nrow(low[low$ret200 > 0, ])/nrow(low)))
positiveRates <- data.frame(positiveRates)
colnames(positiveRates) <- c('60d', '90d', '120d', '200d')
rownames(positiveRates) <- c('NewHigh', 'NewLow')

all <- read.csv('/Users/yang/Downloads/invest/tickers/nasdaq_screener_1671305006609.csv')
marketCap <- NULL
for (ticker in levels(as.factor(high$stock)))
{
  index <- which(all$Symbol == ticker)
  marketCap <- rbind(marketCap, c(ticker, all$Market.Cap[index]))
}
marketCap <- data.frame(marketCap)
colnames(marketCap) <- c('stock', 'marketCap')
marketCap$marketCap <- as.numeric(marketCap$marketCap)/1000000000


tradesByDateHigh <- high %>%
  group_by(date) %>%
  summarise(return200 = sum(ret200)/length(ret200),
            return60 = sum(ret60)/length(ret60),
            return90 = sum(ret90)/length(ret90),
            return120 = sum(ret120)/length(ret120),
            num = length(ret200))

tradesByDateLow <- low %>%
  group_by(date) %>%
  summarise(return200 = sum(ret200)/length(ret200),
            return60 = sum(ret60)/length(ret60),
            return90 = sum(ret90)/length(ret90),
            return120 = sum(ret120)/length(ret120),
            num = length(ret200))

getCumReturns <- function(tradesByDate, interval) {
  cumRet <- NULL
  j <- 1
  while (j < nrow(tradesByDate))
  {
    start <- j
    for (i in start : nrow(tradesByDate)) {
      if (as.Date(tradesByDate$date[i]) >= as.Date(tradesByDate$date[j]) + interval) {
        cumRet <- rbind(cumRet, tradesByDate[i, ])
        j <- i
        break
      }
    }
    j <- j + 1
  }
  return(cumRet)
}

compoundedReturn <- function(data, dates)
{
  eachReturn <- NULL
  total <- 1
  for (i in 1 : length(data))
  {
    total <- total * (1 + data[i])
    eachReturn <- rbind(eachReturn, c(dates[i], total - 1))
  }

  eachReturn <- data.frame(eachReturn)
  colnames(eachReturn) <- c('date', 'return')
  eachReturn$return <- as.numeric(eachReturn$return)
  return(eachReturn)
}

getAllTradesReturns <- function(tradesByDate)
{
  r60 <- getCumReturns(tradesByDate, 60)
  xlist60 <- compoundedReturn(r60$return60, r60$date)
  
  r90 <- getCumReturns(tradesByDate, 90)
  xlist90 <- compoundedReturn(r90$return90, r90$date)
  
  r120 <- getCumReturns(tradesByDate, 120)
  xlist120 <- compoundedReturn(r120$return120, r120$date)
  
  r200 <- getCumReturns(tradesByDate, 200)
  xlist200 <- compoundedReturn(r200$return200, r200$date)
  
  allDays <- c(levels(as.factor(r60$date)),
               levels(as.factor(r90$date)), 
               levels(as.factor(r120$date)),
               levels(as.factor(r200$date)))
  allDays <- levels(as.factor(allDays))
  
  allTrades <- NULL
  for (i in 1 : length(allDays))
  {
    i6 <- which(xlist60$date == allDays[i])
    i9 <- which(xlist90$date == allDays[i])
    i12 <- which(xlist120$date == allDays[i])
    i20 <- which(xlist200$date == allDays[i])
    
    if (length(i6) == 1)
      allTrades <- rbind(allTrades, c(xlist60$date[i6], 
                                      xlist60$return[i6],
                                      0,
                                      0,
                                      0))
    
    if (length(i9) == 1)
      allTrades <- rbind(allTrades, c(xlist90$date[i9],
                                      0,
                                      xlist90$return[i9],
                                      0,
                                      0))
    
    if (length(i12) == 1)
      allTrades <- rbind(allTrades, c(xlist120$date[i12], 
                                      0,
                                      0,
                                      xlist120$return[i12],
                                      0))
    if (length(i20) == 1)
      allTrades <- rbind(allTrades, c(xlist200$date[i20], 
                                      0,
                                      0,
                                      0,
                                      xlist200$return[i20]))
  }
  allTrades <- data.frame(allTrades)
  colnames(allTrades) <- c('date', 'r60', 'r90', 'r120', 'r200')
  allTrades$r60 <- as.numeric(allTrades$r60)
  allTrades$r90 <- as.numeric(allTrades$r90)
  allTrades$r120 <- as.numeric(allTrades$r120)
  allTrades$r200 <- as.numeric(allTrades$r200)
  return (allTrades)
}

allTrades <- getAllTradesReturns(tradesByDateHigh)
plot(allTrades$r60, xaxt = 'n', yaxt = 'n', pch = 20, ylim = c(-1, 15))
points(allTrades$r90, pch = 20, col = 'red')
points(allTrades$r120, pch = 20, col = 'green')
points(allTrades$r200, pch = 19, col = 'blue')
axis(1, at=1:nrow(allTrades), labels = allTrades$date, cex.axis=0.6)
axis(2, at = seq(-1, 15, by = 1), las=2)
legend('topleft', legend=c('Return60', 'Return90', 'Return120', 'Return200'), 
       col = c('black', 'red', 'green', 'blue'), pch = 19, cex = 0.8)

allTradesLow <- getAllTradesReturns(tradesByDateLow)
plot(allTradesLow$r60, xaxt = 'n', yaxt = 'n', pch = 20, ylim = c(-2, 5))
points(allTradesLow$r90, pch = 20, col = 'red')
points(allTradesLow$r120, pch = 20, col = 'green')
points(allTradesLow$r200, pch = 19, col = 'blue')
axis(1, at=1:nrow(allTradesLow), labels = allTradesLow$date, cex.axis=0.6)
axis(2, at = seq(-2, 5, by = 1), las=2)
legend('topleft', legend=c('Return60', 'Return90', 'Return120', 'Return200'), 
       col = c('black', 'red', 'green', 'blue'), pch = 19, cex = 0.8)


spy <- tq_get('SPY',                    
              from = tradesByDateHigh$date[1],
              to = tradesByDateHigh$date[nrow(tradesByDateHigh)],
              get = "stock.prices")

spy$close[nrow(spy)]/spy$close[1] - 1

0.9/(-0.9 + 1)

