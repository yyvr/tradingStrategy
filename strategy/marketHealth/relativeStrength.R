library(tidyquant)
today <- as.character(Sys.Date())
spy <- tq_get('SPY', from = '2022-03-01', to = today, get = "stock.prices")
stock <- tq_get('GOLD', from = '2022-03-01', to = today, get = "stock.prices")
stock$rs <- spy$close[1]/stock$close[1]*stock$close/spy$close - 1
rs <- spy$close[1]/stock$close[1]*stock$close/spy$close - 1
stock$ret <- c(0, round(diff(log(stock$close)), 4))
plot(rs, type = 'l', xaxt = "n")
abline(h=0, col = 'red')
axis(1, at=1:nrow(spy), labels = spy$date, cex.axis=0.6)

var(stock$ret*100)
hist(stock$ret, breaks = 20, xaxt = 'n')
axis(1, at=seq(-0.2:0.15, by = 0.01), cex.axis=0.6)

# Mansfield Relative Performance indicator
today <- as.character(Sys.Date())
spy <- tq_get('SPY', from = '2019-01-01', to = today, get = "stock.prices")
stock <- tq_get('AMAL', from = '2019-01-01', to = today, get = "stock.prices")
stock$rs1 <- stock$close/spy$close * 100
stock$MRP <- (stock$rs1/SMA(stock$rs1, 200) - 1)*100
plot(stock$MRP, type = 'l', xaxt = "n")
abline(h=0, col = 'red')
axis(1, at=1:nrow(spy), labels = spy$date, cex.axis=0.6)

plot(stock$close, type = 'l', xaxt = "n")
axis(1, at=1:nrow(spy), labels = spy$date, cex.axis=0.6)
plot(stock$rs1, type = 'l', xaxt = "n")
points(SMA(stock$rs1, 252), type = 'l', xaxt = "n", col = 'red')
abline(h=0, col = 'red')
axis(1, at=1:nrow(spy), labels = spy$date, cex.axis=0.6)

stock <- tq_get('TELL', from = '2022-04-01', to = today, get = "stock.prices")
stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
var(stock$ret*100)
hist(stock$ret, breaks = 30, xaxt = 'n')
axis(1, at=seq(-0.2:0.15, by = 0.01), cex.axis=0.6)


stock$rs1 <- stock$close/spy$close
plot(stock$close, type = 'l', xaxt = "n")
par(new = TRUE) 
plot(stock$rs1, type = 'l', xaxt = "n", col = 'blue')
points(SMA(stock$rs1, 20), type = 'l', xaxt = "n", col = 'red')
abline(h=0, col = 'red')
axis(1, at=1:nrow(spy), labels = spy$date, cex.axis=0.6)


var(stock$ret*100)
hist(stock$ret, breaks = 20, xaxt = 'n')
axis(1, at=seq(-0.2:0.15, by = 0.01), cex.axis=0.6)

# rolling variance of daily return
stock <- tq_get('PI', from = '2021-03-01', to = today, get = "stock.prices")
stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
data <- data.frame(stock$ret)
rcrr <- rollapply(data, width=60, function(x) sd(x*100), by.column=FALSE)
plot(rcrr, type = 'l', xaxt = 'n')
points(SMA(rcrr, 60), type = 'l', col = 'blue')
axis(1, at=1:nrow(stock), labels = stock$date, cex.axis=0.6)
par(new = TRUE) 
plot(stock$ret, type = 'l', col = 'red', axes = FALSE,xlab = "", ylab = "")
axis(side = 4, at = pretty(range(stock$ret)))
plot(stock$close, type = 'l', col = 'red', axes = FALSE,xlab = "", ylab = "")
plot(SMA(stock$close, 20), type = 'l', col = 'red', axes = FALSE,xlab = "", ylab = "")
axis(side = 4, at = pretty(range(stock$close)))

plot(stock$ret, type = 'l')
abline(h = 0, col = 'red')
abline(h = mean(stock$ret), col = 'green')

stock <- tq_get('TELL', from = '2021-12-01', to = as.character(Sys.Date()), get = "stock.prices")
stock2 <- tq_get('SWN', from = '2021-11-01', to = as.character(Sys.Date()), get = "stock.prices")
stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
stock2$ret <- c(0, round(diff(stock2$close)/stock2$close[2 : nrow(stock2)], 4))
summary(stock$ret)
sd(stock$ret*100)
sd(stock2$ret*100)

t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
}
mycol <- t_col('red', perc = 80, name = 'lt.red')
mycol2 <- t_col('blue', perc = 40, name = 'lt.blue')

hist(stock$ret, col=mycol, axes = FALSE)
hist(stock2$ret, col=mycol2, add=TRUE)
axis(1,at=seq(-0.5,15,by=0.01))
