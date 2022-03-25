library(tidyquant)
library(timetk)
library(formattable)
library(dplyr)
library(data.table)
library(tibble)

files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
start <- '2021-10-01'
end <- '2022-03-24'

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

diff <- 0
adline <- NULL
for (i in 1 : ncol(meta)) {
  x <- meta[, i] > 0
  advance <- sum(x, na.rm = TRUE)
  
  y <- meta[, i] < 0
  decline <- sum(y, na.rm = TRUE)
  diff <- advance - decline + diff
  adline <- c(adline, diff)
}

spy <- tq_get('SPY', from = start, to = end, get = "stock.prices")

x <- spy$date
par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(x, spy$close, type = 'l', col = 'red')              # Create first plot
par(new = TRUE)                             # Add new plot
plot(x, adline, type = 'l', axes = FALSE)
axis(side = 4, at = pretty(range(adline)))      # Add second axis
mtext("advance-decline", side = 4, line = 3)

par(mfrow = c(2, 1))
plot(x, spy$close, type = 'l', col = 'red')
plot(x, adline, type = 'l')

