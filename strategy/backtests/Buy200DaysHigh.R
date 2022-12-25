library(TTR)
files <- list.files(path='/Users/yang/Downloads/invest/usprices2010/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)


allPerform <- NULL
meta <- NULL
for ( i in 1:length(files) ) {
  stock <- read.csv(files[i])
  if (sum(is.na(stock), na.rm = TRUE) != 0) next
  if (nrow(stock) < 200) next
  j = 200

  while (j + 200 < nrow(stock)) {
    if (stock$close[j] * stock$volume[j] < 300000000) {
      j <- j + 1
      next
    }#500m
    
    if (stock$close[j] >= max(stock$close[(j - 200) : j])) {
      ret60 <- stock$close[j + 60]/stock$close[j] - 1
      ret90 <- stock$close[j + 90]/stock$close[j] - 1
      ret120 <- stock$close[j + 120]/stock$close[j] - 1
      ret200 <- stock$close[j + 200]/stock$close[j] - 1
      meta <- rbind(meta, c(stock$date[j], stock$symbol[1], 
                            ret60, ret90, ret120, ret200))
      j <- j + 20
    }
    j <- j + 1
  }
}

meta <- data.frame(meta)
colnames(meta) <- c('date', 'stock', 'ret60', 'ret90', 'ret120', 'ret200')
meta$ret60 <- as.numeric(meta$ret60)
meta$ret90 <- as.numeric(meta$ret90)
meta$ret120 <- as.numeric(meta$ret120)
meta$ret200 <- as.numeric(meta$ret200)

tradesByDate <- meta %>%
  group_by(date) %>%
  summarise(return200 = sum(ret200)/length(ret200),
            return60 = sum(ret60)/length(ret60),
            return90 = sum(ret90)/length(ret90),
            return120 = sum(ret120)/length(ret120),
            num = length(ret200))

getCumReturns <- function(tradesByDate, interval) {
  cumRet <- NULL
  j <- 1
  while (j < nrow(tradesByDate))
  {
    start <- j
    for (i in start : nrow(tradesByDate)) {
      if (as.Date(tradesByDate$date[i]) >= as.Date(tradesByDate$date[j]) + interval) {
        cumRet <- rbind(cumRet, tradesByDate[i, ])
        j <- i
        break
      }
    }
    j <- j + 1
  }
  return(cumRet)
}

ret60 <- getCumReturns(tradesByDate, 60)
ret90 <- getCumReturns(tradesByDate, 90)
ret120 <- getCumReturns(tradesByDate, 120)
ret200 <- getCumReturns(tradesByDate, 200)
sum(ret60$return60)
sum(ret90$return90)
sum(ret120$return120)
sum(ret200$return200)

compoundedReturn <- function(data)
{
  total <- 1
  for(r in data)
  {
    total <- total * (1+r)
    #print(total)
  }
  return(total - 1)
}

print(compoundedReturn(ret60$return60))
print(compoundedReturn(ret90$return90))
print(compoundedReturn(ret120$return120))
print(compoundedReturn(ret200$return200))




write.csv(meta, '/Users/yang/Downloads/invest/strategy/backtests/Buy200DaysHigh.csv')

