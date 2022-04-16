library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
start <- '2020-05-01'
end <- '2022-04-15'

ee <- as.Date('2020-12-03')
counters <- NULL
period <- as.Date(end) -ee
for (i in 1 : period) {
  counters <- rbind(counters, c(as.character(ee), 0))
  ee <- ee + 1
}
counters <- data.frame(counters)
colnames(counters) <- c('date', 'newHigh')
counters$newHigh <- as.numeric(counters$newHigh)
for ( i in 1:length(files) ) {
  print(i)
  stock <- read.csv(files[i])
  tail <- nrow(stock)
  if (is.null(stock) || tail == 0 || tail < 210) next
  
  index <- 0
  for (k in 1 : nrow(counters)) {
    index <- which(stock$date == as.Date(counters$date[k]))
    if (length(index) == 0) next
    if (index > 210)
      break
  }
  
  if (index == 0) next
  for(j in index : tail) {
    if (stock$close[j - 10] < max(stock$close[(j - 210) : (j - 10)]))
      next
    
    if (stock$close[j - 10] > stock$close[j])
      next
    
    id <- which(stock$date[j] == as.Date(counters$date))
    counters$newHigh[id] <- counters$newHigh[id] + 1
  }
}

plot(counters$newHigh, type = 'l', xaxt = 'n')
axis(1, at=1:nrow(counters), labels = counters$date, cex.axis=0.6)

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

ee <- as.Date('2022-01-03')
meta <- NULL
counters <- NULL
for (i in 1 : 10) {
  counters <- rbind(counters, c(as.character(ee), 0))
  cc <- newHighs(start, ee)
  meta <- rbind(meta, c(as.character(ee), cc))
  ee <- ee + 1
}

