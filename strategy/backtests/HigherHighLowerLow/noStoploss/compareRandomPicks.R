source('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/compareResult.R')
high <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysHigh.csv')
low <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysLow.csv')
high$X <- NULL
low$X <- NULL
# in case of too many buying candidates, randomly pick limitNum
pickRandomTradesByDate <- function(allTrades, limitNum, periods)
{
  randomTrades <- NULL
  dates <- levels(as.factor(allTrades$date))
  for (i in 1 : length(dates)) {
    trades <- allTrades[allTrades$date == dates[i], ]
    stocks <- paste(trades$stock, collapse = ', ')
    if (nrow(trades) > 0) {
      rX <- NULL
      if (nrow(trades) > limitNum) {
        RandomNum <- round(runif(limitNum, 1, nrow(trades)))
        for (j in 1 : length(periods)) {
          rX <- c(rX, sum(trades[RandomNum, (j + 2)])/limitNum)
        }
        stocks <- paste(trades$stock[RandomNum], collapse = ', ')
      }
      else {
        for (j in 1 : length(periods)) {
          rX <- c(rX, sum(trades[, (j + 2)])/nrow(trades))
        }
      }
      randomTrades <- rbind(randomTrades, c(dates[i], rX, stocks))
    }
  }
  randomTrades <- data.frame(randomTrades)
  colnames(randomTrades) <- c('date', 'return30', 'return60', 'return90', 'return120', 'return200', 'stocks')
  randomTrades[, 2 : (ncol(randomTrades) - 1)] <- sapply(randomTrades[, 2 : (ncol(randomTrades) - 1)], as.numeric)
  return(randomTrades)
}

periods <- c(30, 60, 90, 120, 200)
tradesByDateHigh <- pickRandomTradesByDate(high, 5, periods)
tradesByDateLow <- pickRandomTradesByDate(low, 5, periods)

# compare against SPY
highSPY <- allResultsDiffStartingDate(tradesByDateHigh, 20, periods)
highSPYRatio <- posRatioAboveSPY(highSPY)
lowSPY <- allResultsDiffStartingDate(tradesByDateLow, 20, periods)
lowSPYRatio <- posRatioAboveSPY(lowSPY)
xx <- merge(highSPYRatio, lowSPYRatio, by = 'holdingPeriod')
colnames(xx) <- c('holdingPeriod', 'ratioAboveSPY.higherHigh', 'ratioAboveSPY.lowerLow')

random1 <- xx

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

plotAllCompoundedReturns(randomTrades, '200day new high strategy Random')

0.1/(-0.1 + 1)
0.2/(-0.2 + 1)
0.3/(-0.3 + 1)
0.5/(-0.5 + 1)
0.9/(-0.9 + 1)

# percentage down <-> percentage up to break even
downRate <- seq(-1, 0, 0.01)
upBreakEvenRate <- -x/(x + 1)
plot(downRate, upBreakEvenRate, type = 'l')

