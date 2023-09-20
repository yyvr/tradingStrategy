files <- list.files(path='/Users/yang/Downloads/invest/weeklyPick/', 
                    pattern=NULL, all.files=FALSE, full.names=TRUE)

stock_files <- list.files(path='/Users/yang/Downloads/invest/usprices/', 
                          pattern=NULL, all.files=FALSE, full.names=TRUE)
start <- '2023-07-02' 
today <- as.character(Sys.Date())

allTrades <- NULL
all <- NULL
for (i in 1:length(files) ) {
  picks <- read.csv(files[i])
  meta <- NULL
  
  for (j in 1 : nrow(picks)) {
    fileName <- paste('/Users/yang/Downloads/invest/usprices/', picks$stock[j], sep = '')
    prices <- read.csv(fileName)
    
    #prices <- tq_get(picks$stock[j], from = start, to = today, get = "stock.prices")
    tail <- nrow(prices)
    index <- which(prices$date == picks$buyDate[j])
    print(index)
    meta <- rbind(meta,
                  c(picks$stock[j], picks$buyDate[j], 
                    as.character(prices$date[tail]), 
                    prices$close[tail]/picks$buyPrice[j] - 1,
                    prices$close[(index + 5)]/picks$buyPrice[j] - 1))
  }
  meta <- data.frame(meta)
  colnames(meta) <- c('stock', 'buyDate', 'sellDate', 'ret', 'ret5')
  all <- rbind(all, c(picks$buyDate[1], 
                      mean(as.numeric(meta$ret)), 
                      mean(as.numeric(meta$ret5))))
  allTrades <- rbind(allTrades, meta)
}

all <- data.frame(all)
colnames(all) <- c('date', 'ret', 'ret5')
all$ret <- round(as.numeric(all$ret)*100, 4)
all$ret5 <- round(as.numeric(all$ret5)*100, 4)
allTrades$ret <- round(as.numeric(allTrades$ret)*100, 4)
allTrades$ret5 <- round(as.numeric(allTrades$ret5)*100, 4)

allTrades <- allTrades[allTrades$buyDate < '2023-09-16', ]
pos <- allTrades[allTrades$ret > 0, ]
nrow(pos)/nrow(allTrades)

tt <- allTrades[allTrades$buyDate < '2023-09-15', ]
#tt <- tt[tt$buyDate > '2023-08-25', ]

allpicks <- NULL
for ( i in 1:length(files) ) {
  picks <- read.csv(files[i])
  allpicks <- rbind(allpicks, picks)
}
unique_stocks <- unique(allpicks$stock)

p <- ((1+all$ret5[1]/100)*(1+all$ret5[2]/100)*(1+all$ret5[3]/100) - 1)*100
  