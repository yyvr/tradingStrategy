# buy the day after red candle turns to green, sell when green->red, 
# and range is above average
library(TTR)
fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)
results <- NULL

tradeColorChange <- function(stock)
{
  buyOpen <- NULL
  sellClose <- NULL
  buydate <- NULL
  selldate <- NULL
  buyrange <- NULL
  sellrange <- NULL
  buyvol <- NULL
  sellvol <- NULL
  buytailrange <- NULL
  buyheadrange <- NULL
  for (i in 1:(nrow(stock) - 4))
  {
    if (stock$color[i] == 'red' & stock$color[i + 1] == 'green')
    {
      # | stock$tailrange[i + 1] < 0.5
      if (stock$range[i + 1] < 1 | stock$vol[i + 1] < 0.8 | stock$headrange[i + 1] > 0.9) next
      for (j in (i + 3) : (nrow(stock) - 1))
      {
        if (stock$color[j] == 'green' & 
            stock$color[j + 1] == 'red' & stock$range[j + 1] >= 1)
          break
      }
      # buy at the third day open price
      buyOpen <- c(buyOpen, stock$Open[i + 2])
      buydate <- c(buydate, stock$Date[i + 2])
      buyrange <- c(buyrange, stock$range[i + 1])
      buytailrange <- c(buytailrange, stock$tailrange[i + 1])
      buyheadrange <- c(buyheadrange, stock$headrange[i + 1])
      buyvol <- c(buyvol, stock$vol[i + 1])
      sellClose <- c(sellClose, stock$Close[j + 1])
      selldate <- c(selldate, stock$Date[j + 1])
      sellrange <- c(sellrange, stock$range[j + 1])
      sellvol <- c(sellvol, stock$vol[j + 1])
    }
  }
  if (is.null(selldate)) return (NULL)
  roc <- (sellClose - buyOpen)/buyOpen
  interval <- as.Date(selldate) - as.Date(buydate)
  x <- data.frame(buydate, buyOpen, buyrange, buyvol, buytailrange, buyheadrange,
                  selldate, sellClose, sellrange, sellvol,
                  roc, interval)
  return (x)
}

addColumes <- function(stock)
{
  arange <- mean(abs(stock$Close - stock$Open))
  stock$range <- abs(stock$Close - stock$Open)/arange
  stock$vol <- stock$Volume/mean(stock$Volume)
  stock$color <- 'red'
  stock[stock$Close > stock$Open, ]$color <- 'green'
  stock$tail <- 0
  stock$head <- 0
  for (j in 1:nrow(stock))
  {
    if (stock$Close[j] > stock$Open[j])
    {
      stock$tail[j] <- stock$Open[j] - stock$Low[j]
      stock$head[j] <- stock$High[j] - stock$Close[j]
    }
    else
    {
      stock$tail[j] <- stock$Close[j] - stock$Low[j]
      stock$head[j] <- stock$High[j] - stock$Open[j]
    }
  }
  stock$tailrange <- stock$tail/mean(stock$tail)
  stock$headrange <- stock$head/mean(stock$head)
  return (stock)
}

all <- NULL
alltrades <- NULL
meta <- NULL
for (i in 1:length(file_list))
{
  #if (file_list[i] != 'OUT1V.HE.txt') next
  stock <- read.csv(file_list[i], stringsAsFactors = FALSE)
  #stock <- stock[as.Date(stock$Date) >= '2018-01-01', ]
  #print(file_list[i])
  if (file_list[i] == 'KESKOB.HE.txt')
  {
    stock[stock$Adj.Close > 20, ]$Adj.Close <- stock[stock$Adj.Close > 20, ]$Adj.Close/4
    stock[stock$Close > 20, ]$Close <- stock[stock$Close > 20, ]$Close/4
    stock[stock$Open > 20, ]$Open <- stock[stock$Open > 20, ]$Open/4
  }
  
  stock <- addColumes(stock)
  x <- tradeColorChange(stock)
  if (sum(x$roc) > 0.3)
  {
    meta <- rbind(meta, c(file_list[i], sum(x$roc)))
    alltrades <- rbind(alltrades, cbind(file_list[i], x))
  }
}
alltrades$buydate <- as.Date(as.character(alltrades$buydate))

buysignal <- NULL
sellsignal <- NULL
latest <- NULL
for (i in 1:nrow(meta))
{
  stock <- read.csv(meta[i], stringsAsFactors = FALSE)
  stock <- addColumes(stock)
  tail <- nrow(stock)
  if (stock$color[tail] == 'green' & stock$color[tail - 1] == 'red' &
      stock$range[tail] > 1 & stock$vol[tail] > 0.8 & stock$headrange[tail] < 0.9)
  {
    buysignal <- rbind(buysignal, c(meta[i], as.character(as.Date(stock$Date[tail]) + 1)))
    latest <- rbind(latest, cbind(meta[i], stock[tail, ]))
  }
  
  if (stock$color[tail] == 'red' & stock$color[tail - 1] == 'green' &
      stock$range[tail] > 1 & stock$vol[tail] > 0.8)
    sellsignal <- rbind(sellsignal, c(meta[i], as.character(as.Date(stock$Date[tail]) + 1)))
}
