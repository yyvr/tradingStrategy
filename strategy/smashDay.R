# close > previous day close, buy if next day gets higher than smash day high
addColumes <- function(stock)
{
  stock$body <- abs(stock$Close - stock$Open)
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
  return (stock)
}

findNakedCloseDay <- function(stock, start)
{
  nakedday <- NULL
  for (i in 6 : (nrow(stock) - 2))
  {
    if (stock$Close[i] < stock$Close[i - 1] & # smash naked close day
        stock$Close[i - 1] < stock$Close[i - 2] &
        stock$Close[i - 2] < stock$Close[i - 3] &
        stock$Close[i] < stock$Close[i - 5])
    {
      # next day is a hammer with long tail but short head
      if (2 * stock$body[i + 1] <= stock$tail[i + 1] &
          stock$head[i + 1] <= 0.02)
      {
        nakedday <- rbind(nakedday, i + 1)
        print(c('Hammer1', as.character(stock$Date[i + 1])))
      }
      if (stock$High[i] < stock$High[i + 1] &
          stock$tail[i + 1] > stock$head[i + 1] &
          stock$color[i + 1] == 'green')
        nakedday <- rbind(nakedday, i + 1)
    }
  }
  return (nakedday)
}

findNakedSellDay <- function(stock, start, interval)
{
  for (j in start : (nrow(stock) - 1))
  {
    if (stock$Close[j] > stock$High[j - 1] &
        #stock$Close[j] > stock$Close[j - interval] &
        stock$Low[j] > stock$Low[j + 1])
    {
      return (j + 1)
    }
  }
  return (nrow(stock))
}

tradeNakedCloseDay <- function(stock, start)
{
  nakedday <- findNakedCloseDay(stock, start)
  buy <- stock[nakedday, ]
  sell <- NULL
  selldate <- NULL
  stopLossRate <- 0.97   
  for (i in nakedday)
  {
    bp <- stock$Close[i]
    exit <- stock$Low[i]
    for (j in (i + 1) : (nrow(stock)))
    {
      #if ((j - i) > 10) break
      if (stock$Close[j] < exit)
      {
        break
      }
      
      if (stock$Close[j] > bp)
        exit <- stock$Low[j]
      
      if (stock$Close[j] > 1.06 * bp)
      {
        break
      }
    }

    sell <- c(sell, stock$Close[j])
    selldate <- c(selldate, as.character(stock$Date[j]))
  }
  #print(c(nrow(buy), length(sell)))
  trades <- data.frame(buy$Date, buy$Close/buy$Open - 1, stock$Low[nakedday + 1]/buy$Close - 1,buy$Close, 
                       as.Date(selldate), sell, sell/buy$Close - 1, 
                       as.Date(selldate) - buy$Date)
  if (nrow(trades) != 0)
    colnames(trades) <- c('buyDate', 'BuyColor', 'nextdayreturn','buyPrice', 
                          'sellDate', 'sellPrice', 'roc', 'interval')
  return (trades)
}

fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)
all <- NULL
meta <- NULL
interval <- 20
for (i in 1 : num_stocks)
{
  #if (file_list[i] != 'SAMPO.HE.txt') next
  if (file_list[i] == '^OMXH25.txt') next
  stock <- read.csv(file_list[i])
  stock$Date <- as.Date(stock$Date)
  stock <- addColumes(stock)
  #stock <- stock[-c(findInsideDays(stock)), ]
  #stock$roc <- c(0, diff(stock$Close)/stock$Close[1 : (nrow(stock) - 1)])
  tr <- tradeNakedCloseDay(stock, interval)
  #nd <- findNakedCloseDay(stock, interval)
  
  if (nrow(tr) != 0)
  {
    
    loss <- tr[tr$roc < 0, ]
    gain <- tr[tr$roc > 0, ]
    #if (nrow(loss) < 4)
    if (sum(tr$roc) > 0.3)
    {
      all <- rbind(all, cbind(file_list[i], tr))
      meta <- rbind(meta, c(file_list[i], sum(tr$roc), nrow(loss), nrow(tr), 
                            nrow(loss)/nrow(tr), sum(loss$roc), sum(gain$roc),
                            stock$Close[nrow(stock)]/stock$Close[1] - 1,
                            mean(tr$interval))) 
    }
  }
}
loss$weekday <- weekdays(loss$buyDate)
gain$weekday <- weekdays(gain$buyDate)
colnames(meta) <- c('stock', 'totalRet', 'numLossTrades', 'numTotalTrades', 
                    'lossRatio', 'loss', 'gain', 'buyHoldRet', 'averageHoldingTime')
newloss <- NULL
for (i in 1 : nrow(loss))
{
  index <- which(loss$buyDate[i] == stock$Date)
  newloss <- rbind(newloss, stock[c((index - 1), index, (index + 1)), ])
}

write.csv(all, '/Users/yang/Downloads/invest/smashday.csv')

