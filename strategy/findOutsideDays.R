library(TTR)
fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)

today <- NULL
start <- 20
for (i in 1:num_stocks)
{
  if (file_list[i] == '^OMXH25.txt') next
  stock <- read.csv(file_list[i])
  stock$Date <- as.Date(stock$Date)
  #stock[, 2:7] <- sapply(stock[, 2:7], as.numeric)
  
  n <- nrow(stock)
  if (stock$High[n] > stock$High[n - 1] &
      stock$Low[n] < stock$Low[n - 1] &
      stock$Close[n] < stock$Close[n - 1] &
      stock$Close[n] < stock$Close[n - start]) # confirm down trend)
  {
    today <- rbind(today, cbind(file_list[i], stock[n, ]))
  }
}