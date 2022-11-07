files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
all <- read.csv('/Users/yang/Downloads/invest/all.csv')

volatilityByInterval <- function(intervals) {
  meta <- NULL
  for ( i in 1:length(files) ) {
    stock <- read.csv(files[i])

    if (sum(is.na(stock), na.rm = TRUE) != 0) next
    if (any(is.na(stock$adjusted))) next
    if (is.null(stock)) next
    if (nrow(stock) < max(intervals)) next

    tail <- nrow(stock)
    stock$ret <- 0
    stock$ret[2 : tail] <- (diff(stock$close)/stock$close[1 : (tail - 1)] - 1)*100
    
    vols <- NULL
    for (period in intervals) {
      head <- tail - period
      vols <- c(vols, round(sd(stock$ret[head : tail]), 4))
    }
    meta <- rbind(meta, c(stock$symbol[1], vols, stock$close[tail] * stock$volume[tail]))
    #break
  }
  return (meta)
}

allVolatilities <- data.frame(volatilityByInterval(c(30, 60, 90, 120)))
colnames(allVolatilities) <- c('stock', 'v30', 'v60', 'v90', 'v120', 'turnOver')
allVolatilities$v30 <- as.numeric(allVolatilities$v30)
allVolatilities$v60 <- as.numeric(allVolatilities$v60)
allVolatilities$v90 <- as.numeric(allVolatilities$v90)
allVolatilities$v120 <- as.numeric(allVolatilities$v120)
allVolatilities$turnOver <- as.numeric(allVolatilities$turnOver)

activeStocks <- allVolatilities[allVolatilities$v30 > 1, ]
activeStocks <- activeStocks[activeStocks$turnOver > 10000000, ]
