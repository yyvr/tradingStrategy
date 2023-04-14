high <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysHigh.csv')
low <- read.csv('/Users/yang/Downloads/invest/strategy/backtests/HigherHighLowerLow/noStoploss/Buy200DaysLow.csv')

#high <- low
tradesByDateHigh <- low %>%
  group_by(date) %>%
  summarise(return30 = sum(ret30)/length(ret30),
            return60 = sum(ret60)/length(ret60),
            return90 = sum(ret90)/length(ret90),
            return120 = sum(ret120)/length(ret120),
            return200 = sum(ret200)/length(ret200),
            num = length(ret200))
deriveStakes <- function(data)
{
  stakes <- NULL
  for (i in 2 : ncol(data))
  {
    posRate <- sum(data[, i] > 0, na.rm = TRUE)/nrow(data)
    posRet <- colMeans(data[data[, i] > 0, i])
    negRate <- sum(data[, i] < 0, na.rm = TRUE)/nrow(data)
    negRet <- colMeans(data[data[, i] < 0, i])
    stakes <- c(stakes, posRate/(-negRet) - negRate/posRet)
  }
  return (stakes)
}

optimalStakes <- function(high)
{
  stakesHigh <- NULL
  i <- 1
  while (i < 2000)
  {
    stakesHigh <- rbind(stakesHigh, c(high$date[i], deriveStakes(high[ i : nrow(high), ])))
    i <- i + 20
  }
  stakesHigh <- data.frame(stakesHigh)
  colnames(stakesHigh) <- c('date', 'ret30', 'ret60', 'ret90', 'ret120', 'ret200')
  
  for (i in 2 : ncol(stakesHigh)) {
    stakesHigh[, i] <- as.numeric(stakesHigh[, i])
  }
  
  return(stakesHigh)
}

stakesHigh <- optimalStakes(tradesByDateHigh)

library(plotly)
fig <- plot_ly()
fig <- add_trace(fig, x = ~stakesHigh$date, 
                 y = ~stakesHigh$ret30, type = 'scatter', mode = 'markers',
                 name = 'ret30')
fig <- add_trace(fig, x = ~stakesHigh$date, y = ~stakesHigh$ret60, 
                 type = 'scatter', mode = 'markers', name = 'ret60')
fig <- add_trace(fig, x = ~stakesHigh$date, y = ~stakesHigh$ret90, 
                 type = 'scatter', mode = 'markers', name = 'ret90')
fig <- add_trace(fig, x = ~stakesHigh$date, y = ~stakesHigh$ret120, 
                 type = 'scatter', mode = 'markers', name = 'ret120')
fig <- add_trace(fig, x = ~stakesHigh$date, y = ~stakesHigh$ret200, 
                 type = 'scatter', mode = 'markers', name = 'ret200') %>%
  layout(legend=list(title=list(text='Legend')),
         xaxis = list(title = 'Date',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         yaxis = list(title = 'bet size',
                      zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
         plot_bgcolor='#e5ecf6')
fig

