findImpliedSmashBuy <- function(stock)
{
  impliedsmash <- NULL
  for (j in 2 : (nrow(stock) - 2))
  {
    if (stock$Close[j] > stock$Close[j - 1] &
        (stock$Close[j] - stock$Low[j]) <= 0.3 * (stock$High[j] - stock$Low[j]) &
        stock$High[j + 1] > stock$High[j] &
        stock$High[j] >= stock$Close[j - 1])
        #stock$Close[j] < stock$Close[j - 10])
    {
      impliedsmash <- rbind(impliedsmash, j + 1)
    }
  }
  return (impliedsmash)
}
today <- NULL

tradeImpliedSmash <- function(stock)
{
  smashdays <- findImpliedSmashBuy(stock)
  buy <- stock[smashdays, ]
  sell <- NULL
  selldate <- NULL
  stopLossRate <- 0.95
  
  for (i in smashdays)
  {
    bp <- stock$Close[i]
    for (j in (i + 1) : (nrow(stock)))
    {
      if (bp * stopLossRate > stock$Close[j])
      {
        break
      }
      else if (stock$Close[j] > 1.015 * bp)
      {
        break
      }
    }
    
    sell <- c(sell, stock$Close[j])
    selldate <- c(selldate, as.character(stock$Date[j]))
  }
  trades <- data.frame(buy$Date, buy$Close/buy$Open, buy$Close, 
                       as.Date(selldate), sell, sell/buy$Close - 1, 
                       as.Date(selldate) - buy$Date)
  if (nrow(trades) != 0)
    colnames(trades) <- c('buyDate', 'BuyColor', 'buyPrice', 
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
for (i in 1:num_stocks)
{
  #if (file_list[i] != 'TIETO.HE.txt') next
  if (file_list[i] == '^OMXH25.txt') next
  stock <- read.csv(file_list[i])
  stock$Date <- as.Date(stock$Date)
  tr <- tradeImpliedSmash(stock)
  #nd <- findNakedCloseDay(stock, interval)
  
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