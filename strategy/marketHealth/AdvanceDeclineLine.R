library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
start <- '2021-05-01'
end <- '2022-04-08'

dailyReturn <- function(start, end) {
  meta <- NULL
  for ( i in 1:length(files) ) {
    stock <- read.csv(files[i])
    stock <- stock[as.Date(stock$date) <= as.Date(end) & as.Date(stock$date) >= as.Date(start), ]
    if (is.null(stock) || nrow(stock) == 0) next
    stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
    meta <- rbind(meta, c(stock$ret))
  }
  meta <- data.frame(meta)
  colnames(meta) <- c(stock$date)
  return (meta)
}

meta <- dailyReturn(start, end)

spy <- tq_get('SPY', from = start, to = end, get = "stock.prices")
diff <- 0
adline <- NULL
adResult <- data.frame(spy$date, 0, 0)
colnames(adResult) <- c('date', 'advance', 'decline')
for (i in 1 : ncol(meta)) {
  x <- meta[, i] > 0
  advance <- sum(x, na.rm = TRUE)
  
  y <- meta[, i] < 0
  decline <- sum(y, na.rm = TRUE)
  diff <- advance - decline + diff
  adline <- c(adline, diff)
  adResult$advance[i] <- advance
  adResult$decline[i] <- decline
}

adResult$diff <- adResult$advance - adResult$decline
adResult$cumSum <- cumsum(adResult$diff)

#adresult <- data.frame(stock$date, adline)
#write.csv(adresult, '/Users/yang/Downloads/invest/advanceDecline20150101To20220325.csv', row.names = FALSE)

x <- spy$date
plot(spy$close, type = 'l', col = 'red', xaxt = "n")
par(new = TRUE) 
plot(adline, type = 'l', xaxt = "n")
par(new = TRUE) 
plot(SMA(spy$close, 60), type = 'l', col = 'blue', xaxt = "n")
axis(1, at=1:length(x), labels = spy$date, cex.axis=0.6)

# rolling correlation
#data <- data.frame(spy$close, adResult$cumSum)
#rcrr <- rollapply(data, width=30, function(x) cor(x[,1],x[,2]), by.column=FALSE)
par(mfrow = c(2, 1))
plot(spy$close, type = 'l', col = 'red', xaxt = "n")
axis(1, at=1:length(spy$date), labels = spy$date, cex.axis=0.6)
plot(adline, type = 'l', xaxt = "n")
axis(1, at=1:length(spy$date), labels = spy$date, cex.axis=0.6)
#plot(rcrr, type = 'l', xaxt = 'n')
#axis(1, at=1:nrow(data), labels = spy$date, cex.axis=0.6)
#abline(h=0)

# momentum index, very long term
plot(SMA(adline, 200), type = 'l', col = 'blue', xaxt = 'n')
axis(1, at=1:length(x), labels = spy$date, cex.axis=0.4)
abline(h=0)



