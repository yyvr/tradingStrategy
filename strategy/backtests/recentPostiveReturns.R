library(TTR)
files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
all <- read.csv('/Users/yang/Downloads/invest/tickers/nasdaq_screener_1670612069900.csv')
all <- data.frame(all)

spy <- tq_get('SPY',                    
              from = '2020-01-01',
              to = Sys.Date(),
              get = "stock.prices")

allPerform <- NULL
meta <- NULL
for ( i in 1:length(files) ) {
  stock <- read.csv(files[i])
  if (sum(is.na(stock), na.rm = TRUE) != 0) next
  for (j in seq(1, 50, 10)) {
    end <- spy$date[150 + j]
    index <- which(as.character(stock$date) == end)
    if (length(index) != 1)
      next
    
    if (nrow(stock) < (index + 90)) next
    if (index <= 20) next
    
    ret20 <- stock$close[index]/stock$close[index - 20] - 1
    ret10 <- stock$close[index]/stock$close[index - 10] - 1
    retAfter10 <- stock$close[index + 10]/stock$close[index] - 1
    retAfter20 <- stock$close[index + 20]/stock$close[index] - 1
    retAfter90 <- stock$close[index + 90]/stock$close[index] - 1
    
    meta <- rbind(meta, c(stock$date[index], stock$symbol[1], 
                          ret10, ret20, retAfter10, retAfter20, retAfter90))
  }
}

meta <- data.frame(meta)
colnames(meta) <- c('date', 'stock', 'ret10', 'ret20', 'retAfter10', 'retAfter20', 'retAfter90')
meta$ret10 <- as.numeric(meta$ret10)
meta$ret20 <- as.numeric(meta$ret20)
meta$retAfter10 <- as.numeric(meta$retAfter10)
meta$retAfter20 <- as.numeric(meta$retAfter20)
meta$retAfter90 <- as.numeric(meta$retAfter90)

allPerform <- NULL

dates <- levels(as.factor(meta$date))
for (d in dates) {
  allReturns <- meta[meta$date == d, ]
  allReturns$ret10 <- as.numeric(allReturns$ret10)
  allReturns$ret20 <- as.numeric(allReturns$ret20)
  allReturns$retAfter10 <- as.numeric(allReturns$retAfter10)
  allReturns$retAfter20 <- as.numeric(allReturns$retAfter20)
  allReturns$retAfter90 <- as.numeric(allReturns$retAfter90)
  
  negReturns <- allReturns[allReturns$ret10 < 0 | allReturns$ret20 < 0, ]
  positiveReturns <- allReturns[allReturns$ret10 > 0 & allReturns$ret20 > 0, ]
  
  allPerform <- rbind(allPerform, c(d,
                                    sum(negReturns$retAfter10)/nrow(negReturns),
                                    sum(negReturns$retAfter20)/nrow(negReturns),
                                    sum(negReturns$retAfter90)/nrow(negReturns),
                                    sum(positiveReturns$retAfter10)/nrow(positiveReturns),
                                    sum(positiveReturns$retAfter20)/nrow(positiveReturns),
                                    sum(positiveReturns$retAfter90)/nrow(positiveReturns)))
}



allPerform <- data.frame(allPerform)
colnames(allPerform) <- c('date', 'n10', 'n20', 'n90', 'p10', 'p20', 'p90')
allPerform$n10 <- as.numeric(allPerform$n10)
allPerform$n20 <- as.numeric(allPerform$n20)
allPerform$n90 <- as.numeric(allPerform$n90)
allPerform$p10 <- as.numeric(allPerform$p10)
allPerform$p20 <- as.numeric(allPerform$p20)
allPerform$p90 <- as.numeric(allPerform$p90)
allPerform$diff10 <- allPerform$p10 - allPerform$n10
allPerform$diff20 <- allPerform$p20 - allPerform$n20
allPerform$diff90 <- allPerform$p90 - allPerform$n90
sum(allPerform$diff10)
sum(allPerform$diff20)
sum(allPerform$diff90, na.rm = TRUE)

pp <- allPerform[allPerform$diff10 > 0, ]
