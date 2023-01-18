library(TTR)
files <- list.files(path='/Users/yang/Downloads/invest/usprices2010/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)

meta <- NULL
holdingPeriods <- c(30, 60, 90, 120, 200)
holdingPeriodsName <- c('ret30', 'ret60', 'ret90', 'ret120', 'ret200')
for ( i in 1:length(files) ) {
  stock <- read.csv(files[i])
  if (sum(is.na(stock), na.rm = TRUE) != 0) next
  if (nrow(stock) < 200) next
  j = 200
  
  maxHolding <- max(holdingPeriods)
  while (j + maxHolding < nrow(stock)) {
    if (stock$close[j] * stock$volume[j] < 300000000) {
      j <- j + 1
      next
    }#300m
    
    # past 200-day new high
    if (stock$close[j] >= max(stock$close[(j - 200) : j])) {
      returns <- NULL
      for(k in 1 : length(holdingPeriods)) {
        ret <- stock$close[j + holdingPeriods[k]]/stock$close[j] - 1
        returns <- c(returns, ret)
      }
      
      meta <- rbind(meta, c(stock$date[j], stock$symbol[1], returns))
      j <- j + 20
    }
    j <- j + 1
  }
}

meta <- data.frame(meta)
colnames(meta) <- c('date', 'stock', holdingPeriodsName)
meta[holdingPeriodsName] <- sapply(meta[holdingPeriodsName], as.numeric)

write.csv(meta, 
          '/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysHigh.csv')