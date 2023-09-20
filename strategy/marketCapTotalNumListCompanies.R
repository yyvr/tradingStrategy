library(stringr)
library(anytime)
files <- list.files(path='/Users/yang/Downloads/invest/tickers/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)

totalCaps <- NULL
numStocks <- NULL
dates <- NULL
for ( i in 3:length(files) ) {
  stock <- read.csv(files[i])
  stock <- stock[stock$Market.Cap > 0 & !is.na(stock$Market.Cap), ]
  
  date <- str_split(files[i], '_', simplify = TRUE)[3]
  dd <- str_split(date, '\\.', simplify = TRUE)[1]
  dates <- c(dates, 
             as.character(as.Date(as.POSIXct(as.numeric(dd)/1000, origin = "1970-01-01"))))
  totalCaps <- c(totalCaps, sum(stock$Market.Cap, na.rm = TRUE))
  numStocks <- c(numStocks, nrow(stock))
  print(nrow(stock))
}

meta <- data.frame(dates, totalCaps, numStocks)

par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(totalCaps/1000000000, xaxt = 'n', pch = 20, 
     xlab = 'Date', ylab = 'Total market cap in billions')# Create first plot
par(new = TRUE)                             # Add new plot
plot(numStocks, xaxt = 'n', pch = 19, yaxt = 'n', ylab = '',
     axes = FALSE, col = 'red', xlab = '')
axis(side = 4, at = pretty(range(numStocks)))      # Add second axis
mtext("Total number of stocks", side = 4, line = 3)
axis(1, at=1:length(dates), labels = dates, cex.axis=0.6)
legend('top', legend=c("market cap", "number of stocks"),
       col=c("black", "red"), pch=20:19, cex=0.5)

