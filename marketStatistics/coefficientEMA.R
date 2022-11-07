library(TTR)
files <- list.files(path='/Users/yang/Downloads/invest/usprices/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
all <- read.csv('/Users/yang/Downloads/invest/all.csv')

coefficientByInterval <- function(intervals) {
  meta <- NULL
  for ( i in 1:length(files) ) {
    stock <- read.csv(files[i])
    print(stock$symbol[1])
    if (sum(is.na(stock), na.rm = TRUE) != 0) next
    if (any(is.na(stock$adjusted))) next
    if (is.null(stock)) next
    if (nrow(stock) < max(intervals) + 50) next
    
    tail <- nrow(stock)
    
    stock$ret <- 0
    stock$ema60 <- EMA(stock$close, 60)
    stock$ret[2 : tail] <- (diff(stock$close)/stock$close[1 : (tail - 1)] - 1)*100
    
    coefficients <- NULL
    for (period in intervals) {
      head <- tail - period
      result <- summary(lm(stock$ema60[head : tail] ~ c(1 : (period + 1))))$coefficients
      coefficients <- c(coefficients, round(result[2, 1], 4))
    }
    meta <- rbind(meta, c(stock$symbol[1], coefficients, stock$close[tail] * stock$volume[tail]))
    #break
  }
  return (meta)
}

allCoefficients <- data.frame(coefficientByInterval(c(30, 60, 90, 120)))
colnames(allCoefficients) <- c('stock', 'v30', 'v60', 'v90', 'v120', 'turnOver')
allCoefficients$v30 <- as.numeric(allCoefficients$v30)
allCoefficients$v60 <- as.numeric(allCoefficients$v60)
allCoefficients$v90 <- as.numeric(allCoefficients$v90)
allCoefficients$v120 <- as.numeric(allCoefficients$v120)
allCoefficients$turnOver <- as.numeric(allCoefficients$turnOver)

uptrendStocks <- allCoefficients[allCoefficients$v30 > 0, ]
uptrendStocks <- uptrendStocks[uptrendStocks$v60 > 0, ]
uptrendStocks <- uptrendStocks[uptrendStocks$v90 > 0, ]
uptrendStocks <- uptrendStocks[uptrendStocks$turnOver > 10000000, ]

#tail <- nrow(stock)
#head <- tail - 60
#stock$ema60 <- EMA(stock$close, 60)
#result <- summary(lm(stock$ema60[head : tail] ~ c(1 : (60 + 1))))$coefficients