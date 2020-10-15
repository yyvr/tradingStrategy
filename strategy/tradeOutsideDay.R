tradeOutsideDay <- function(stock, start)
{
  buy <- NULL
  sell <- NULL
  selldate <- NULL
  stopLossRate <- 0.8 
  for (i in (start + 1) : (nrow(stock) - 2))
  {
    if (stock$High[i] > stock$High[i - 1] &
        stock$Low[i] < stock$Low[i - 1] &
        stock$Close[i] < stock$Close[i - 1] &
        stock$Close[i] < stock$Close[i - start] #&  confirm down trend
        #stock$Close[i] > stock$Open[i + 1]
        )
    {
      buy <- rbind(buy, stock[i + 1, ])
      bp <- stock$Open[i + 1]

      found <- 0
      for (j in (i + 2) : nrow(stock))
      {
        if (bp * stopLossRate > stock$Low[j])
        {
          if (bp * stopLossRate < stock$Close[j])
            sell <- c(sell, bp * stopLossRate)
          else
            sell <- c(sell, stock$Close[j])
          found <- 1
          break
        }
        else if (stock$Close[j] > 1.015 * bp)
        {
          found <- 1
          sell <- c(sell, stock$Close[j])
          break
        }
      }
      if (found == 0)
        sell <- c(sell, stock$Close[j])
      selldate <- c(selldate, as.character(stock$Date[j]))
    }
  }
  trades <- data.frame(buy$Date, buy$Open, 
                       as.Date(selldate), sell, sell/buy$Open - 1, 
                       as.Date(selldate) - buy$Date)
  if (nrow(trades) != 0)
    colnames(trades) <- c('buyDate', 'buyOpenPrice', 
                       'sellDate', 'sellClosePrice', 'roc', 'interval')
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
  #if (file_list[i] != 'UPM.HE.txt') next
  if (file_list[i] == '^OMXH25.txt') next
  stock <- read.csv(file_list[i])
  stock$Date <- as.Date(stock$Date)
  stock$roc <- c(0, diff(stock$Close)/stock$Close[1 : (nrow(stock) - 1)])
  tr <- tradeOutsideDay(stock, interval)
  if (nrow(tr) != 0)
  {
    
    loss <- tr[tr$roc < 0, ]
    gain <- tr[tr$roc > 0, ]
    #if (nrow(loss) == 0)
    if (sum(tr$roc) > 0.2)
    {
      all <- rbind(all, cbind(file_list[i], tr))
      meta <- rbind(meta, c(file_list[i], sum(tr$roc), nrow(loss), nrow(tr), 
                            nrow(loss)/nrow(tr), sum(loss$roc), sum(gain$roc),
                            stock$Close[nrow(stock)]/stock$Close[1] - 1,
                            mean(tr$interval))) 
    }
  }
}
colnames(meta) <- c('stock', 'totalRet', 'numLossTrades', 'numTotalTrades', 
                    'lossRatio', 'loss', 'gain', 'buyHoldRet', 'averageHoldingTime')


