library(TTR)
fp <- "/Users/yang/Downloads/invest/prices"
setwd(fp)
file_list <- list.files(path = fp)
num_stocks <- length(file_list)

# find outside days
findOutsideDay <- function(stock)
{
  outside <- NULL
  for (i in 2 : (nrow(stock) - 1))
  {
    if (stock$High[i] > stock$High[i - 1] &
        stock$Low[i] < stock$Low[i - 1] &
        stock$Close[i] < stock$Close[i - 1] &
        stock$Close[i] > stock$Open[i + 1])
    {
      outside <- rbind(outside, stock[i + 1, ])
    }
  }
  return (outside)
}

all <- NULL
today <- NULL
for (i in 1:num_stocks)
{
  if (file_list[i] == '^OMXH25.txt') next
  stock <- read.csv(file_list[i])
  stock$Date <- as.Date(stock$Date)
  stock[, 2:7] <- sapply(stock[, 2:7], as.numeric)
  n <- nrow(stock)
  if (stock$High[n] > stock$High[n - 1] &
      stock$Low[n] < stock$Low[n - 1] &
      stock$Close[n] < stock$Close[n - 1])
  {
    today <- rbind(today, cbind(file_list[i], stock[n, ]))
  } 
  
  all <- rbind(all, cbind(file_list[i], findOutsideDay(stock)))
}

library('lubridate')
for (i in 2016 : 2020)
{
  tmp <- stock[year(stock$Date) == i, ]
  for (j in 1 : 12)
  {
    mtmp <- tmp[month(tmp$Date) == j, ]
    print(c(i, j, nrow(mtmp)))
  }
}