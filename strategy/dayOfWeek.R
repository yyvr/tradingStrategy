# Test buy on certain day of the week and sell in the next 5 trading days
stock <- read.csv('/Users/yang/Downloads/FORTUM-2015-01-01-2020-10-02.csv')
stock <- stock[order(as.Date(stock$Date)), ]
stock$roc <- c(0, diff(stock$Close)/stock$Close[1 : (nrow(stock) - 1)])
stock$Date <- as.Date(stock$Date)
days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

# buy at start date, sell at end date, 
# set negret to 0 if only want buy price < previous day
buySell <- function(stock, start, end, negret)
{
  buy <- NULL
  sell <- NULL
  for (i in 1:(nrow(stock) - 1))
  {
    if (weekdays(stock$Date[i]) == start & stock$roc[i] < negret)
    {
      buy <- rbind(buy, stock[i, ])
      sell <- rbind(sell, stock[(i + end), ])
    }
  }
  trades <- data.frame(buy$Date, buy$Close, sell$Date, sell$Close, sell$Close/buy$Close - 1)
  colnames(trades) <- c('buyDate', 'buyPrice', 'sellDate', 'sellPrice', 'roc')
  return(trades)
}

results <- matrix(0, 5, 5)
colnames(results) <- days
rownames(results) <- days
for (i in 1 : 5)
{
  tmp <- NULL
  for (j in 1 : 4)
  {
    tr <- buySell(stock, days[i], j, 0)
    if ((i + j) >= 6) index <- (i + j) %% 6 + 1
    else index <- i + j
    tmp <- c(tmp, index)
    results[i, index] <- sum(tr$roc, na.rm = TRUE)
    print(c(days[i], j, nrow(tr[tr$roc > 0.02, ])/nrow(tr), sum(tr$roc, na.rm = TRUE)))
  }
}

tr <- buySell(stock, 'Friday', 3, 0)
sum(tr$roc)
sum(tr[tr$roc < 0, ]$roc)
sum(tr[tr$roc > 0, ]$roc)
posrate <- nrow(tr[tr$roc > 0, ])/nrow(tr)
negrate <- nrow(tr[tr$roc < -0.05, ])/nrow(tr)
hist(tr$roc, breaks = 40, xaxt='n')
axis(1, at=seq(-0.2, 0.2, 0.01))
