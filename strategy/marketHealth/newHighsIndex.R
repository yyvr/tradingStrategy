library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

files <- list.files(path='/Users/yang/Downloads/invest/usprices2016/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)

end <- as.character(Sys.Date())
start <- '2016-01-01'
spy <- tq_get('SPY', from = start, to = end, get = "stock.prices")
counters <- NULL
counters <- data.frame(spy$date, 0, 0)
colnames(counters) <- c('date', 'newHigh', 'newLow')

for ( i in 1: length(files)) { #
  stock <- read.csv(files[i])
  tail <- nrow(stock)
  if (is.null(stock) | tail == 0 | tail < 210) next
  
  print(i)
  index <- 0
  for (k in 1 : nrow(counters)) {
    index <- which(stock$date == as.Date(counters$date[k]))
    if (length(index) == 0) next
    if (index > 210)
      break
  }
  
  if (index == 0 | length(index) == 0) next
  for(j in index : tail) {
    if (stock$close[j - 10] >= max(stock$close[(j - 210) : (j - 10)]) &
        stock$close[j - 10] < stock$close[j]) {
      id <- which(stock$date[j] == as.Date(counters$date))
      counters$newHigh[id] <- counters$newHigh[id] + 1
    }
    
    if (stock$close[j - 10] <= min(stock$close[(j - 210) : (j - 10)]) &
        stock$close[j - 10] > stock$close[j]) {
      id <- which(stock$date[j] == as.Date(counters$date))
      counters$newLow[id] <- counters$newLow[id] + 1
    }
  }
}

counters$diff <- counters$newHigh - counters$newLow
write.csv(counters, file = '/Users/yang/Downloads/invest/newHighsIndexSince2016.csv')

spy$ma150 <- SMA(spy$close, 150)
spy$ma60 <- SMA(spy$close, 60)

ss <- 1
ee <- nrow(spy)
plot(spy$close[ss : ee], type = 'l', col = 'red', axes = FALSE,xlab = "", ylab = "")
axis(side = 4, at = pretty(range(spy$close)))
points(spy$ma60[ss: ee], type = 'l', col = 'blue', xaxt = "n")
points(spy$ma150[ss : ee], type = 'l', col = 'green', xaxt = "n")
par(new = TRUE) 
plot(counters$diff[ss : ee], type = 'l', xaxt = "n")
abline(h=100, col = 'blue')
axis(1, at=1:(ee - ss + 1), labels = spy$date[ss : ee], cex.axis=0.6)

hist(counters$diff, breaks = 30, xaxt = 'n')
axis(side=1, at=seq(-1550,900, 50), labels=seq(-1550,900, 50))

newHighs <- function() {
  counter <- 0
  for ( i in 1:length(files) ) {
    print(i)
    stock <- read.csv(files[i])
    tail <- nrow(stock)
    if (is.null(stock) || tail == 0) next
    index <- nrow(stock)#which(stock$date == as.Date(counters$date[1]))
    if (length(index) == 0 || index < 211) next
    if (stock$close[index - 10] < max(stock$close[(index - 210) : (index - 10)]))
      next
    
    if (stock$close[index - 10] > stock$close[index])
      next
    counter <- counter + 1
  }
  return(counter)
}

cc <- newHighs()


