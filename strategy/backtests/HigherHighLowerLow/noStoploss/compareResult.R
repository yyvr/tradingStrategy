testDifferentStartingDate2 <- function(tradesByDate, periods)
{
  allPerformance <- NULL
  
  for (i in 1 : length(periods)) {
    rX <- getCumReturns(tradesByDate, periods[i])
    if (nrow(rX) > 0) {
      allPerformance <- rbind(allPerformance, 
                              c(periods[i], 
                                getFinalCompoundedReturn(unlist(rX[, (i + 1)]), rX$date)))
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

allResultsDiffStartingDate <- function(tradesByDate, interval, periods)
{
  spy <- tq_get('SPY',                    
                from = '2010-01-01',
                to = Sys.Date(),
                get = "stock.prices")
  allP <- NULL
  for (i in seq(1, nrow(tradesByDate), interval)) {
    allP <- 
      rbind(allP, 
            testDifferentStartingDate2(tradesByDate[i : nrow(tradesByDate), ], periods))
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