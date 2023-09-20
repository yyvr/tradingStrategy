files <- list.files(path='/Users/yang/Downloads/invest/usprices/', 
                    pattern=NULL, all.files=FALSE, full.names=TRUE)
all <- read.csv('/Users/yang/Downloads/invest/tickers/nasdaq_screener_1694844542515.csv')

start <- '2020-01-02' 
today <- as.character(Sys.Date())
spy <- tq_get('SPY', from = start, to = today, get = "stock.prices")

findBuyCandidate <- function(stock, interval) {
  meta <- NULL
  
  head <- tail - interval
  tail <- nrow(stock)
  
  stock$ret <- 0
  stock$ret[2 : tail] <- (diff(stock$close)/stock$close[1 : (tail - 1)] - 1)*100
  
  # price > ema60
  x <- stock$close[head : tail] < EMA(stock$close, 60)[head : tail]
  if (sum(x, na.rm = TRUE) > 1) {
    return (meta)
  }
  
  if (EMA(stock$adjusted, 20)[tail] < EMA(stock$adjusted, 30)[tail])
    return (meta)
  if (EMA(stock$adjusted, 30)[tail] < EMA(stock$adjusted, 40)[tail])
    return (meta)
  if (EMA(stock$adjusted, 40)[tail] < EMA(stock$adjusted, 50)[tail])
    return (meta)
  if (EMA(stock$adjusted, 50)[tail] < EMA(stock$adjusted, 60)[tail])
    return (meta)
  if (EMA(stock$adjusted, 60)[tail] < EMA(stock$adjusted, 120)[tail])
    return (meta)
  if (EMA(stock$adjusted, 60)[head] < EMA(stock$adjusted, 120)[head])
    return (meta)
  if (EMA(stock$adjusted, 120)[tail] < EMA(stock$adjusted, 120)[head])
    return (meta)
  
  if (stock$adjusted[tail] < EMA(stock$adjusted, 20)[tail])
    return (meta)
  
  # too long rally should avoid buying
  if (tail > 200) {
    x <- EMA(stock$adjusted, 120)[(tail - 200) : tail] < 
      EMA(stock$adjusted, 60)[(tail - 200) : tail]
    print(c(stock$symbol[1], sum(x, na.rm = TRUE), stock$date[tail]))
    
    if (sum(x, na.rm = TRUE) >= 200) {
      #print(c(stock$symbol[1]), sum(x, na.rm = TRUE))
      print(c(4, stock$date[head], stock$date[tail]))
      #next
    }
  }
  
  meta <- rbind(meta, c(stock$symbol[1],
                        stock$date[tail],
                        stock$close[tail],
                        EMA(stock$adjusted, 20)[tail]/EMA(stock$adjusted, 60)[tail],
                        stock$low[tail]/EMA(stock$close, 20)[tail],
                        stock$volume[tail] * stock$adjusted[tail]/1000000,
                        sd(stock$ret[head : tail]),
                        stock$close[tail]/stock$close[tail - 10] - 1,
                        stock$volume[tail]/mean(stock$volume[(tail - 60) : tail])))
  return (meta)
}

meta <- NULL
interval <- 22
for ( i in 1:length(files) ) {
  #if (!grepl('ELF', files[i], fixed = TRUE))next
  stock <- read.csv(files[i])
  print(c(files[i]))
  #if (stock$symbol[1] != 'ELF') next
  if (nrow(stock) < 200) next
  if (is.null(stock)) next
  
  tail <- nrow(stock)
  if (stock$volume[tail] * stock$close[tail] < 20000000)
    next
  if (sum(is.na(stock), na.rm = TRUE) != 0) next
  if (any(is.na(stock$adjusted))) next
  
  meta <- rbind(meta, findBuyCandidate(stock, interval))
}
meta <- data.frame(meta)

#turnover in millions
if (nrow(meta) > 0) {
  colnames(meta) <- c('stock', 'buyDate', 'buyPrice',
                      'ema20/60', 'low/ema20', 'turnOver', 'volatility', 
                      'tenRet', 'relativeVolume')
}
meta$turnOver <- as.numeric(meta$turnOver)
meta$volatility <- as.numeric(meta$volatility)
meta$`ema20/60` <- as.numeric(meta$`ema20/60`)
meta$`low/ema20` <- as.numeric(meta$`low/ema20`)
meta$tenRet <- round(as.numeric(meta$tenRet), 4)

filtered <- meta[meta$volatility > 1, ]
filtered <- filtered[filtered$`low/ema20` > 1.005, ]
filtered <- filtered[filtered$buyPrice > 10, ]
filtered$marketCap <- 0
filtered$industry <- ''
filtered$sector <- ''
for (i in 1 : nrow(filtered))
{
  index <- which(all$Symbol == filtered$stock[i])
  if (length(index) == 0)
    next
  filtered$marketCap[i] <- all$Market.Cap[index]
  filtered$industry[i] <- all$Industry[index]
  filtered$sector[i] <- all$Sector[index]
} 
filtered <- filtered[filtered$marketCap > 0, ]
filtered$marketCap <- round(filtered$marketCap/1000000000, 4)
filtered$buyPrice <- as.numeric(filtered$buyPrice)

newHighs <- NULL
# check if the chosen stocks made 200-day new high
for (i in 1 : nrow(filtered))
{
  fileName <- paste('/Users/yang/Downloads/invest/usprices/', filtered$stock[i], sep = '')
  stock <- read.csv(fileName)
  tail <- nrow(stock)
  if (max(stock$close[(tail - 10) : tail]) >= max(stock$close[(tail - 200) : tail])) {
    newHighs <- rbind(newHighs, filtered[i, ])
  }
}

newHighs <- newHighs[newHighs$relativeVolume > 1, ]
newHighs$MRP <- 0
for (i in 1 : nrow(newHighs))
{
  fileName <- paste('/Users/yang/Downloads/invest/usprices/', filtered$stock[i], sep = '')
  stock <- read.csv(fileName)
  
  tail <- which(stock$date == spy$date[nrow(spy)])
  head <- which(spy$date == stock$date[1])
  spytmp <- spy[head : nrow(spy), ]
  stock <- stock[1 : tail, ]
  if (nrow(spytmp) != nrow(stock))
    break
  print(c(fileName, nrow(spytmp), nrow(stock)))
  stock$rs1 <- stock$close/spytmp$close * 100
  MRP <- (stock$rs1/SMA(stock$rs1, 200) - 1)*100
  newHighs$MRP[i] <- MRP[tail]
}
newHighs$`ema20/60` <- NULL
newHighs <- newHighs[newHighs$relativeVolume > 1.03, ]

newHighs <- newHighs[newHighs$marketCap > 1, ]

filtered2 <- filtered[filtered$turnOver > 200, ]
filtered2 <- filtered2[filtered2$marketCap > 10, ]
output <- data.frame(filtered2$stock, filtered2$buyDate, filtered2$buyPrice)
colnames(output) <- c('stock', 'buyDate', 'buyPrice')
write.csv(filtered2$stock, '/Users/yang/Downloads/invest/stocks.txt', row.names = FALSE)
write.csv(output, '/Users/yang/Downloads/invest/weeklyPick/20230915.txt', row.names = FALSE)

  