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

addSPYReturns <- function(tradesByDate)
{
  spy <- tq_get('SPY',                    
                from = tradesByDate$date[1],
                to = tradesByDate$date[nrow(tradesByDate)],
                get = "stock.prices")
  for(i in 2 : nrow(tradesByDate))
  {
    index <- which(spy$date == tradesByDate$date[i])
    if(length(index) == 1)
      tradesByDate$spyReturn[i] <- spy$close[index]/spy$close[1] - 1
  }
  return(tradesByDate)
}

plotAllCompoundedReturns <- function(tradesByDate, name)
{
  allTrades <- getAllTradesReturns(tradesByDate)
  allTrades$spyReturn <- 0
  allTrades <- addSPYReturns(allTrades)
  
  maxValue <- max(allTrades$r60, allTrades$r90, allTrades$r120, allTrades$r200)
  plot(allTrades$r60, xaxt = 'n', yaxt = 'n', pch = 20, ylim = c(-1, maxValue),
       xlab = name, ylab = 'Compounded returns')
  points(allTrades$r90, pch = 20, col = 'red')
  points(allTrades$r120, pch = 20, col = '#7CCD7C')
  points(allTrades$r200, pch = 20, col = 'blue')
  points(allTrades$spyReturn, pch = 20, col = '#CDAD00')
  axis(1, at=1:nrow(allTrades), labels = allTrades$date, cex.axis=0.6)
  axis(2, at = seq(-1, maxValue, by = 1))
  legend('topleft', legend=c('Return60', 'Return90', 'Return120', 'Return200', 'SPY Return'), 
         col = c('black', 'red', '#7CCD7C', 'blue', '#CDAD00'), pch = 19, cex = 0.8)
  abline(h=-0.5, col = 'red')
}

plotAllCompoundedReturns(tradesByDateHigh, '200day new high strategy')
plotAllCompoundedReturns(tradesByDateLow, '200day new low strategy')

r60 <- getCumReturns(tradesByDateHigh, 60)
xlist60 <- compoundedReturn(r60$return60, r60$date)

r90 <- getCumReturns(tradesByDateHigh, 90)
xlist90 <- compoundedReturn(r90$return90, r90$date)

r120 <- getCumReturns(tradesByDateHigh, 120)
xlist120 <- compoundedReturn(r120$return120, r120$date)

r200 <- getCumReturns(tradesByDateHigh, 200)
xlist200 <- compoundedReturn(r200$return200, r200$date)

r60 <- getCumReturns(tradesByDateLow, 60)
xlist60 <- compoundedReturn(r60$return60, r60$date)

r90 <- getCumReturns(tradesByDateLow, 90)
xlist90 <- compoundedReturn(r90$return90, r90$date)

r120 <- getCumReturns(tradesByDateLow, 120)
xlist120 <- compoundedReturn(r120$return120, r120$date)

r200 <- getCumReturns(tradesByDateLow, 200)
xlist200 <- compoundedReturn(r200$return200, r200$date)

finalResult <- data.frame(r60 = xlist60$return[nrow(xlist60)],
                          r90 = xlist90$return[nrow(xlist90)],
                          r120 = xlist120$return[nrow(xlist120)],
                          r200 = xlist200$return[nrow(xlist200)])
finalResult <- rbind(finalResult, c(xlist60$return[nrow(xlist60)],
                                    xlist90$return[nrow(xlist90)],
                                    xlist120$return[nrow(xlist120)],
                                    xlist200$return[nrow(xlist200)]))

rownames(finalResult) <- c('NewHigh', 'NewLow')
allTrades <- getAllTradesReturns(tradesByDateHigh)

# in case of too many buying candidates, randomly pick limitNum
pickRandomTradesByDate <- function(allTrades, limitNum)
{
  randomTrades <- NULL
  dates <- levels(as.factor(allTrades$date))
  for (i in 1 : length(dates)) {
    trades <- allTrades[allTrades$date == dates[i], ]
    stocks <- paste(trades$stock, collapse = ', ')
    if (nrow(trades) > 0) {
      if (nrow(trades) > limitNum) {
        RandomNum <- round(runif(limitNum, 1, nrow(trades)))
        r60 <- sum(trades$ret60[RandomNum])/limitNum
        r90 <- sum(trades$ret90[RandomNum])/limitNum
        r120 <- sum(trades$ret120[RandomNum])/limitNum
        r200 <- sum(trades$ret200[RandomNum])/limitNum
        stocks <- paste(trades$stock[RandomNum], collapse = ', ')
      }
      else {
        r60 <- sum(trades$ret60)/nrow(trades)
        r90 <- sum(trades$ret90)/nrow(trades)
        r120 <- sum(trades$ret120)/nrow(trades)
        r200 <- sum(trades$ret200)/nrow(trades)
      }
      randomTrades <- rbind(randomTrades, c(dates[i], r60, r90, r120, r200, stocks))
    }
  }
  randomTrades <- data.frame(randomTrades)
  colnames(randomTrades) <- c('date', 'return60', 'return90', 'return120', 'return200', 'stocks')
  randomTrades$return60 <- as.numeric(randomTrades$return60)
  randomTrades$return90 <- as.numeric(randomTrades$return90)
  randomTrades$return120 <- as.numeric(randomTrades$return120)
  randomTrades$return200 <- as.numeric(randomTrades$return200)
  return(randomTrades)
}

randomTrades <- pickRandomTradesByDate(high, 5)
allRandomTrades <- getAllTradesReturns(randomTrades)
allRandomTrades$stocks <- NULL
for (i in 1 : nrow(allRandomTrades)) {
  index <- which(allRandomTrades$date[i] == randomTrades$date)
  allRandomTrades$stocks[i] <- randomTrades$stocks[index]
}
randomTrades <- pickRandomTradesByDate(low, 10)
allRandomTrades <- getAllTradesReturns(randomTrades)
plotAllCompoundedReturns(randomTrades, '200day new high strategy Random')

0.1/(-0.1 + 1)
0.2/(-0.2 + 1)
0.5/(-0.5 + 1)
0.9/(-0.9 + 1)

# percentage down <-> percentage up to break even
x <- seq(-1, 0, 0.01)
y <- -x/(x + 1)
plot(x, y)

