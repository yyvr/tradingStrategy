library(TTR)
fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)

numShares <- read.csv('/Users/yang/Downloads/invest/getStockData/numShares.txt')
marketCap <- NULL
for (i in 1:num_stocks)
{
  print(file_list[i])
  shares <- numShares[numShares$tiker == file_list[i], ]
  if (nrow(shares) != 0)
  {
    stock <- read.csv(file_list[i])
    tail <- nrow(stock)
    marketCap <- rbind(marketCap, c(file_list[i], stock$Close[tail] * shares$shares))
  }
}
marketCap <- data.frame(marketCap)
# market cap in millions
marketCap$X2 <- as.numeric(marketCap$X2)/1000000
colnames(marketCap) <- c('stock', 'marketCap')
