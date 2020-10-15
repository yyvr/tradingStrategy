# for i in `cat tickers.txt`; do ./get15-20.sh $i; done
# buy the biggest losers of a day and sell when price is up
library(TTR)
fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)
results <- NULL

findSellDate <- function(data, ratio, rise)
{
  n <- nrow(data)
  r <- diff(data$Close)/data$Close[1:(n-1)]
  #r <- (data$Low[2:n] - data$Close[1:(n-1)])/data$Close[1:(n-1)]
  index <- which(r <= ratio) + 1
  gains <- NULL
  buydate <- NULL
  selldate <- NULL
  buyprice <- NULL
  sellprice <- NULL
  drop <- NULL
  vol <- NULL
  for (i in index)
  {
    if ((i + 1) > n) break
    if(data$Volume[i]/mean(data$Volume) <= 1) next
    for (j in (i+1):n)
    {
      if (data$Close[j] > rise * data$Close[i]) break
    }
    buydate <- c(buydate, as.character(data$Date[i]))
    selldate <- c(selldate, as.character(data$Date[j]))
    #buyprice <- c(buyprice, data$Low[i])
    buyprice <- c(buyprice, data$Close[i])
    sellprice <- c(sellprice, data$Close[j])
    #sellprice <- c(sellprice, data$High[j])
    drop <- c(drop, r[i-1])
    vol <- c(vol, data$Volume[i]/mean(data$Volume))
  }
  roc <- (sellprice - buyprice)/buyprice
  #if (length(roc) == 0) return (NULL)
  rr <- data.frame(buydate, buyprice, selldate, sellprice, roc, drop, vol)
  return (rr)
}

getOptimalDrop <- function(data, dropupper, riseupper)
{
  max <- 0
  drop <- 0
  rise <- 0
  for (i in 1:dropupper)
  {
    for (j in 1:riseupper)
    {
      r <- findSellDate(data, -1 * i/200, (1 + j/200))
      gain <- sum(as.numeric(r$roc))
      if (gain > max)
      {
        max <- gain
        drop <- -1 * i/200#
        rise <- 1+ j/200
      }      
    }
  }
  return (c(drop, rise))
}

extractTrades <- function(stock, drop, rise)
{
  results <- findSellDate(stock, drop, rise)
  results$buydate <- as.Date(as.character(results$buydate))
  results$selldate <- as.Date(as.character(results$selldate))
  results$intervals <- results$selldate - results$buydate
  return (results)
}

realizedTrades <- function(data)
{
  realized <- NULL
  for(i in 2 : nrow(data))
  {
    if (data$buydate[i] < data$selldate[i - 1]) next
    realized <- rbind(realized, data[i, ])
  }
  return (realized)
}

omxh <- read.csv('^OMXH25.txt')
#omxh <- omxh[as.Date(omxh$Date) > '2020-04-01', ]
omxh$indexroc <- c(0, round(diff(omxh$Close)/omxh$Close[1:(nrow(omxh)-1)], 3))
x <- omxh[omxh$indexroc <= -0.02, ]
x$interval <- c(0, diff(as.Date(x$Date)))
meta <- NULL
all <- NULL
for (i in 1:num_stocks)
{
  if (file_list[i] == '^OMXH25.txt') next
  stock <- read.csv(file_list[i])
  stock <- stock[as.Date(stock$Date) > '2020-04-01', ]
  stock[, 2:7] <- sapply(stock[, 2:7], as.numeric)
  #stock$omxhroc <- omxh$indexroc
  print(c(file_list[i], stock[as.Date(stock$Date) == '2020-09-21', ]$Close))
  ratios <- getOptimalDrop(stock, 20, 20)
  #alltrades <- extractTrades(stock, -0.04, ratios[2])
  alltrades <- extractTrades(stock, ratios[1], ratios[2])
  finaltrades <- alltrades[alltrades$drop <= median(alltrades$drop), ]
  finaltrades$stock <- file_list[i]
  ss <- sum(finaltrades$roc)
  if (ss > 0.5)
    meta <- rbind(meta, c(file_list[i], round(median(alltrades$drop), 4), ss, 
                          median(finaltrades$intervals), 
                          median(finaltrades$roc)))
  all <- rbind(all, finaltrades)
}
colnames(meta) <- c('stock', 'mediandrop', 'accumreturn', 'medianInterval', 'medianreturn')

#stock <- read.csv(file_list[file_list == 'VALMT.HE.txt'])
#stock <- stock[as.Date(stock$Date) > '2020-03-20', ]
#rt <- realizedTrades(alltrades)
#print(sum(rt$roc))

if(FALSE) {
rl <- (stock$Low[2:nrow(stock)] - stock$Close[1:(nrow(stock) - 1)])/stock$Close[1:(nrow(stock) - 1)]
x <- cbind(as.character(stock$Date[2:nrow(stock)]), rl)
hist(rl, breaks = 40, xaxt='n')
axis(1, at=seq(-0.2, 0.2, 0.01))

r <- (stock$High[2:nrow(stock)] - stock$Close[1:(nrow(stock) - 1)])/stock$Close[1:(nrow(stock) - 1)]
x <- cbind(as.character(stock$Date[2:nrow(stock)]), r)
hist(r, breaks = 20, xaxt='n')
axis(1, at=seq(-0.2, 0.2, 0.01))

r1 <- diff(stock$Adj.Close)/stock$Adj.Close[1:(nrow(stock) - 1)]
y <- cbind(as.character(stock$Date)[2:nrow(stock)], r1)
hist(r1, breaks = 30, xaxt='n')
axis(1, at=seq(-0.2, 0.2, 0.01))

r <- (stock$High[2:nrow(stock)] - stock$Close[1:(nrow(stock) - 1)])/stock$Close[1:(nrow(stock) - 1)]
z <- cbind(as.character(stock$Date[2:nrow(stock)]), r)
}
