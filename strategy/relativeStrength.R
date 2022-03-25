stock <- tq_get('MO', from = '2021-08-01', to = '2022-03-24', get = "stock.prices")
spy <- tq_get('SPY', from = '2021-08-01', to = '2022-03-24', get = "stock.prices")
stock$rs <- spy$close[1]/stock$close[1]*stock$close/spy$close - 1
rs <- spy$close[1]/stock$close[1]*stock$close/spy$close - 1
stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
plot(rs, type = 'l')
abline(h=0, col = 'red')
var(stock$ret*100)



stock <- tq_get('MO', from = '2021-12-01', to = '2022-03-24', get = "stock.prices")
stock2 <- tq_get('ENPH', from = '2021-11-01', to = '2022-03-22', get = "stock.prices")
stock$ret <- c(0, round(diff(stock$close)/stock$close[2 : nrow(stock)], 4))
stock2$ret <- c(0, round(diff(stock2$close)/stock2$close[2 : nrow(stock2)], 4))
summary(stock$ret)
var(stock$ret*100)

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

hist(stock$ret, col=mycol2, xlim=c(-0.05, 0.1), breaks = 20)

axis(1,at=seq(-0.5,15,by=0.005))

hist(stock2$ret, col=mycol, add=TRUE, breaks = 20)
