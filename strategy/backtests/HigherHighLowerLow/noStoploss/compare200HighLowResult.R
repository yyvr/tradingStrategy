high <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysHigh.csv')
low <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysLow.csv')
positiveRates <- NULL
positiveRates <- rbind(positiveRates, c(sum(high$ret30 > 0)/nrow(high),
                                        sum(high$ret60 > 0)/nrow(high),
                                        sum(high$ret90 > 0)/nrow(high),
                                        sum(high$ret120 > 0)/nrow(high),
                                        sum(high$ret200 > 0)/nrow(high)))
positiveRates <- rbind(positiveRates, c(sum(low$ret30 > 0)/nrow(low),
                                        sum(low$ret60 > 0)/nrow(low),
                                        sum(low$ret90 > 0)/nrow(low),
                                        sum(low$ret120 > 0)/nrow(low),
                                        sum(low$ret200 > 0)/nrow(low)))
positiveRates <- data.frame(positiveRates)
colnames(positiveRates) <- c('30d', '60d', '90d', '120d', '200d')
rownames(positiveRates) <- c('NewHigh', 'NewLow')

# average return by date, equal allocation
tradesByDateHigh <- high %>%
  group_by(date) %>%
  summarise(return30 = sum(ret30)/length(ret30),
            return60 = sum(ret60)/length(ret60),
            return90 = sum(ret90)/length(ret90),
            return120 = sum(ret120)/length(ret120),
            return200 = sum(ret200)/length(ret200),
            num = length(ret200))

tradesByDateLow <- low %>%
  group_by(date) %>%
  summarise(return30 = sum(ret30)/length(ret30),
            return60 = sum(ret60)/length(ret60),
            return90 = sum(ret90)/length(ret90),
            return120 = sum(ret120)/length(ret120),
            return200 = sum(ret200)/length(ret200),
            num = length(ret200))

testDifferentStartingDate2 <- function(tradesByDate)
{
  allPerformance <- NULL
  for (i in c(30, 60, 90, 120, 200)) {
    rX <- getCumReturns(tradesByDate, i)
    if (nrow(rX) > 0) {
      if (i == 30) {
        allPerformance <- rbind(allPerformance, 
                                c(i, getFinalCompoundedReturn(rX$return30, rX$date)))
      }
      
      if (i == 60)
        allPerformance <- rbind(allPerformance, 
                                c(i, getFinalCompoundedReturn(rX$return60, rX$date)))
      
      if (i == 90)
        allPerformance <- rbind(allPerformance, 
                                c(i, getFinalCompoundedReturn(rX$return90, rX$date)))
      
      if (i == 120)
        allPerformance <- rbind(allPerformance, 
                                c(i, getFinalCompoundedReturn(rX$return120, rX$date)))
      
      if (i == 200)
        allPerformance <- rbind(allPerformance, 
                                c(i, getFinalCompoundedReturn(rX$return200, rX$date)))
    }
  }
  allPerformance <- data.frame(allPerformance)
  colnames(allPerformance) <- c('holdingPeriod', 'start', 'end', 'finalReturn')
  return (allPerformance)
}

# find next trade after the previous trade is closed
getCumReturns <- function(tradesByDate, interval) {
  cumRet <- NULL
  j <- 1
  cumRet <- rbind(cumRet, tradesByDate[1, ])
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

getFinalCompoundedReturn <- function(data, dates)
{
  eachReturn <- NULL
  total <- 1
  for (i in 1 : length(data))
  {
    total <- total * (1 + data[i])
  }
  eachReturn <- c(dates[1], dates[length(dates)], total - 1)
  return(eachReturn)
}

# get all the intermediate returns during the whole test period
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

spy <- tq_get('SPY',                    
              from = '2010-01-01',
              to = Sys.Date(),
              get = "stock.prices")
allResultsDiffStartingDate <- function(tradesByDate, interval)
{
  allP <- NULL
  for (i in seq(1, nrow(tradesByDate), interval)) {
    allP <- 
      rbind(allP, 
            testDifferentStartingDate2(tradesByDate[i : nrow(tradesByDate), ]))
  }
  
  allP$holdingPeriod <- as.numeric(allP$holdingPeriod)
  allP$finalReturn <- as.numeric(allP$finalReturn)
  
  # compare to SPY
  allP$spyReturn <- 0
  for(i in 2 : nrow(allP)) {
    start <- which(spy$date == allP$start[i])
    end <- which(spy$date == allP$end[i])
    if (length(start) == 0)
      print(allP$start[i])
    if(length(start) == 1 && length(end) == 1)
      allP$spyReturn[i] <- spy$close[end]/spy$close[start] - 1
  }
  allP$diff <- allP$finalReturn - allP$spyReturn
  
  return (allP)
}

posRatioAboveSPY <- function(allP)
{
  # ratio (better than SPY)
  performs <- NULL
  for (i in c(30, 60, 90, 120, 200)) {
    all <- allP[allP$holdingPeriod == i, ]
    performs <- rbind(performs, c(i, sum(all$diff > 0)/nrow(all)))
  }
  
  performs <- data.frame(performs)
  colnames(performs) <- c('holdingPeriod', 'ratioAboveSPY')
  return (performs)
}

# compare against SPY
highSPY <- allResultsDiffStartingDate(tradesByDateHigh, 20)
lowSPY <- allResultsDiffStartingDate(tradesByDateLow, 20)
xx <- merge(highSPY, lowSPY, by = 'holdingPeriod')
colnames(xx) <- c('holdingPeriod', 'ratioAboveSPY.higherHigh', 'ratioAboveSPY.lowerLow')


getAllTradesReturns2 <- function(tradesByDate, periods) 
{
  allList <- NULL
  for (i in 1 : length(periods)) {
    r <- getCumReturns(tradesByDate, periods[i])
    if (nrow(r) > 0) {
      xlist <- compoundedReturn(unlist(r[, (i + 1)]), r$date)
      allList <- rbind(allList, cbind(periods[i], xlist))
    }
  }
  
  allDays <- levels(as.factor(allList$date))
  allTrades <- data.frame(date = allDays, 0, 0, 0, 0, 0)
  colnames(allTrades) <- c('date', 'r30', 'r60', 'r90', 'r120', 'r200')
  for (i in 1 : length(allDays))
  {
    for (j in 1 : length(periods)) {
      list <- allList[allList$periods == periods[j], ]
      index <- which(list$date == allDays[i])
      if (length(index) == 1)
        allTrades[i, (j + 1)] <- list[index, 3]
    }
  }
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
  allTrades <- getAllTradesReturns2(tradesByDate, c(30, 60, 90, 120, 200))
  allTrades$spyReturn <- 0
  allTrades <- addSPYReturns(allTrades)
  
  maxValue <- max(allTrades[, c(2 : ncol(allTrades))])
  plot(allTrades$r60, xaxt = 'n', yaxt = 'n', pch = 20, ylim = c(-1, maxValue),
       xlab = name, ylab = 'Compounded returns')
  points(allTrades$r30, pch = 20, col = '#BF3EFF')
  points(allTrades$r90, pch = 20, col = 'red')
  points(allTrades$r120, pch = 20, col = '#7CCD7C')
  points(allTrades$r200, pch = 20, col = 'blue')
  points(allTrades$spyReturn, pch = 20, col = '#CDAD00')
  axis(1, at=1:nrow(allTrades), labels = allTrades$date, cex.axis=0.6)
  axis(2, at = seq(-1, maxValue, by = 1))
  legend('topleft', 
         legend=c('Return30', 'Return60', 'Return90', 'Return120', 'Return200', 'SPY Return'), 
         col = c('#BF3EFF', 'black', 'red', '#7CCD7C', 'blue', '#CDAD00'), 
         pch = 19, cex = 0.8)
  abline(h=-0.5, col = 'red')
}

plotAllCompoundedReturns(tradesByDateHigh, '200day new high strategy')
plotAllCompoundedReturns(tradesByDateLow, '200day new low strategy')

r30 <- getCumReturns(tradesByDateHigh, 30)
xlist30 <- compoundedReturn(r30$return30, r30$date)
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

finalResult <- data.frame(r30 = xlist30$return[nrow(xlist30)],
                          r60 = xlist60$return[nrow(xlist60)],
                          r90 = xlist90$return[nrow(xlist90)],
                          r120 = xlist120$return[nrow(xlist120)],
                          r200 = xlist200$return[nrow(xlist200)])
finalResult <- rbind(finalResult, c(xlist60$return[nrow(xlist60)],
                                    xlist90$return[nrow(xlist90)],
                                    xlist120$return[nrow(xlist120)],
                                    xlist200$return[nrow(xlist200)]))

rownames(finalResult) <- c('NewHigh', 'NewLow')
allTrades <- getAllTradesReturns(tradesByDateHigh)

0.1/(-0.1 + 1)
0.2/(-0.2 + 1)
0.3/(-0.3 + 1)
0.5/(-0.5 + 1)
0.9/(-0.9 + 1)

# percentage down <-> percentage up to break even
downRate <- seq(-1, 0, 0.01)
upBreakEvenRate <- -x/(x + 1)
plot(downRate, upBreakEvenRate, type = 'l')

