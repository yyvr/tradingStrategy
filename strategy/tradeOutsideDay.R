stock <- read.csv('/Users/yang/Downloads/invest/prices/FORTUM.HE.txt')
stock$X <- NULL
stock <- na.omit(stock)
stock$Date <- as.Date(stock$Date)
stock <- stock[order(stock$Date), ]
#stock <- stock[stock$Date >= '2006-07-17', ]
stock$roc <- c(0, diff(stock$Close)/stock$Close[1 : (nrow(stock) - 1)])

buy <- NULL
sell <- NULL
selldate <- NULL
for (i in 2 : (nrow(stock) - 1))
{
  if (stock$High[i] > stock$High[i - 1] &
      stock$Low[i] < stock$Low[i - 1] &
      stock$Close[i] < stock$Close[i - 1] &
      stock$Close[i] > stock$Open[i + 1])
  {
    buy <- rbind(buy, stock[i + 1, ])
    bp <- stock$Open[i + 1]
    for (j in (i + 2) : nrow(stock))
    {
      if (bp * 0.95 > stock$Low[j])
      {
        if (bp * 0.95 < stock$Close[j])
          sell <- c(sell, bp * 0.95)
        else
          sell <- c(sell, stock$Close[j])
        break
      }
      else if (stock$Close[j] > bp)
      {
        sell <- c(sell, stock$Close[j])
        break
      }
    }
    selldate <- c(selldate, as.character(stock$Date[j]))
  }
}
trades <- data.frame(buy$Date, weekdays(buy$Date), buy$Open, 
                     as.Date(selldate), sell, sell/buy$Open - 1, 
                     as.Date(selldate) - buy$Date)

days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
for (i in 1:5)
{
  d <- stock[weekdays(stock$Date) == days[i], ]
  for (j in 2015 : 2020)
  {
    yd <- d[as.numeric(format(d$Date, '%Y')) == j, ]
    a <- yd[yd$Open > 1.006 * yd$Low, ]
    print(c(j, days[i], round(nrow(a)/nrow(yd), 3)))
  }
}

days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
for (i in 1:5)
{
  d <- stock[weekdays(stock$Date) == days[i], ]
  a <- d[d$Open > d$Close, ]
  print(c(days[i], round(nrow(a)/nrow(d), 3)))
}

