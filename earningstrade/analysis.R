library(plotly)
library(tidyquant)
data <- read.csv('/Users/yang/Downloads/invest/refinitiv/earnings_surprises/all_results.csv')
data <- na.omit(data)

# accuracies rate, ret >= 0.05
# when surprisePCT is i% and stock return is j%
accuracies <- NULL
for ( i in 1:10) {
  pos <- data[data$predicted > i, ]
  for ( j in 0:10) {
    posRet <- pos[pos$ret > j/100, ]
    if (nrow(posRet) == 0) break
    accuracies <- rbind(accuracies, c(i, j, nrow(posRet)/nrow(pos)))    
    print(c(nrow(posRet), nrow(pos), i, j))
  }
}
accuracies <- data.frame(accuracies)
colnames(accuracies) <- c('surpricePTC%', 'stockRet%', 'accuracy')

fig <- plot_ly()
for ( i in 0:5) {
  fig <- add_trace(fig, x = accuracies[accuracies$`stockRet%` == i,]$`surpricePTC%`, 
                   y = accuracies[accuracies$`stockRet%` == i,]$accuracy*100, 
                   evaluate = TRUE,
                   name = paste(i, '%'))
}

fig <- add_trace(fig,
  x = c(min(accuracies$`stockRet%`), max(accuracies$`stockRet%`)),  # Cover full x range
  y = c(50, 50),
  type = 'scatter',
  mode = 'lines',
  line = list(color = 'red'),  # Customize line
  name = 'Threshold'
)
fig %>% layout(title='Accuracy rates of predicted positive EPS surprise%',
               legend=list(title=list(text='Stock return')),
               xaxis = list(title = 'predicted positive EPS surprise%',
                            zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
               yaxis = list(title = 'Accuracy rate%',
                            zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
               plot_bgcolor='#e5ecf6') 


failure <- NULL
for ( i in 1:30) {
  neg <- data[data$predicted < i*(-1), ]
  for ( j in 0:30) {
    negRet <- neg[neg$ret < (-1)*j/100, ]
    failure <- rbind(failure, c(i*(-1), (-1)*j, nrow(negRet)/nrow(neg)))
  }
}
failure <- data.frame(failure)
colnames(failure) <- c('surpricePTC%', 'stockRet%', 'accuracy')

fig <- plot_ly()
for ( i in 0:5) {
  fig <- add_trace(fig, x = failure[failure$`stockRet%` == (-1)*i,]$`surpricePTC%`, 
                   y = failure[failure$`stockRet%` == (-1)*i,]$accuracy*100, 
                   evaluate = TRUE,
                   name = paste((-1)*i, '%'))
}
fig <- add_trace(fig,
                 x = c(min(failure$`stockRet%`), max(failure$`stockRet%`)),  # Cover full x range
                 y = c(50, 50),
                 type = 'scatter',
                 mode = 'lines',
                 line = list(color = 'red'),  # Customize line
                 name = 'Threshold'
)
fig %>% layout(title='Accuracy rates of predicted negative EPS surprise%',
               legend=list(title=list(text='Stock return')),
               xaxis = list(title = 'predicted negative EPS surprise%',
                            zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
               yaxis = list(title = 'Accuracy rate%',
                            zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff'),
               plot_bgcolor='#e5ecf6') 


streamline_earnings_trade <- function(target_surprise, data, positive=TRUE) {
  pos <- data[data$predicted > target_surprise, ]
  # negative surprise
  if (positive == FALSE) {
    pos <- data[data$predicted < target_surprise, ]
  }
  
  pos <- na.omit(pos)
  pos$reportDate <- as.Date(pos$reportDate)
  pos <- pos[order(pos$reportDate), ]
  pos$buyDate <- as.Date(pos$buyDate)
  pos$sellDate <- as.Date(pos$sellDate)  
  
  all <- NULL
  allTrades <- NULL
  cost <- 0.4
  for (start in seq(1, nrow(pos), 20)) {
    if (start > nrow(pos)) break
    last_sell <- pos$sellDate[start]
    totalRet <- 1
    for (i in start:nrow(pos)) {
      if (last_sell < pos$buyDate[i]) {
        totalRet <- totalRet * (1 + pos$ret[i] * (1 - cost))
        allTrades <- rbind(allTrades, 
                           c(start, pos$RIC[i], pos$buyDate[i], 
                             pos$sellDate[i], pos$ret[i], pos$predicted_date[i],
                             pos$predicted[i], pos$actual[i]))
      }
      last_sell <- pos$sellDate[i]
    }
    
    all <- rbind(all, c(start, pos$buyDate[start], totalRet - 1))
  }
  all <- data.frame(all)
  colnames(all) <- c('batch', 'buyDate', 'totalRet')
  all$buyDate <- as.Date(all$buyDate)

  qqq <- tq_get('QQQ',                    
                from = as.character(all$buyDate[1]),
                to = as.character(Sys.Date()),
                get = "stock.prices")
  tail <- nrow(qqq)
  all$qqq <- 0
  for (i in 1 : nrow(all)) {
    start <- which(all$buyDate[i] == qqq$date)
    all$qqq[i] <- qqq$close[tail]/qqq$close[start] - 1
  }
  all$diff <- all$totalRet - all$qqq
  
  allTrades <- data.frame(allTrades)
  colnames(allTrades) <- c('batch', 'RIC', 'buyDate', 'sellDate', 
                           'ret', 'predictedDate', 'predicted', 'actual')
  allTrades$buyDate <- as.Date(as.numeric(allTrades$buyDate))
  allTrades$sellDate <- as.Date(as.numeric(allTrades$sellDate))
  allTrades$ret <- as.numeric(allTrades$ret)
  allTrades$predicted <- as.numeric(allTrades$predicted)
  allTrades$actual <- as.numeric(allTrades$actual)
  
  return(list(all = all, allTrades = allTrades))
}

kelly <- function(trades) {
  p <- nrow(subset(trades, ret > 0))/nrow(trades)
  q <- 1 - p
  a <- mean(subset(trades, ret > 0)$ret)
  b <- mean(subset(trades, ret < 0)$ret) * (-1)
  return(p/b - q/a)
}

perf_compare <- NULL
all_aggregate <- NULL
totalTrades <- NULL
kelly_perf <- NULL
for (i in 1 : 20) {
  result <- streamline_earnings_trade(i, data)
  all <- result$all
  allTrades <- result$allTrades
  allTrades <- allTrades[allTrades$batch == 1, ]
  kelly_perf <- rbind(kelly_perf, c(i, kelly(allTrades)))
  perf_compare <- rbind(perf_compare, 
                        c(i, nrow(subset(all, diff > 0))/nrow(all)))
  #all$target_surprise <- i
  #all_aggregate <- rbind(all_aggregate, all)
  
}
kelly_perf <- data.frame(kelly_perf)
perf_compare <- data.frame(perf_compare)
colnames(perf_compare) <- c('target_surprise', 'beatQQQ')
perf_compare$kelly <- kelly_perf$X2

result <- streamline_earnings_trade(9, data, TRUE)
all <- result$all
allTradesFive <- result$allTrades
allTradesFive <- allTradesFive[allTradesFive$batch == 1, ]
plot(allTradesFive$buyDate, allTradesFive$ret)
hist(allTradesFive$ret)
kelly(allTradesFive)


first <- allTrades[allTrades$batch == 1, ]
a <- diff(first$buyDate)
print(a)


diff <- (data$actual - data$predicted)
filtered <- diff[diff <= 300 & diff >= -300]

library(ggplot2)
fig <- plot_ly()
p <- ggplot(data.frame(values = filtered) , aes(x = values)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.6, position = "identity") +
  geom_vline(xintercept = 0, color = "red", size = 1) +
  labs(title = "Distribution of the difference between actual and predicted", 
       x = "Actual surprise - predicted", y = "Count")
fig <- ggplotly(p)
fig


normal <- data[data$actual > -100 & data$actual < 100 & data$predicted > -10 & data$predicted < 10, ]
plot(normal$predicted, normal$actual)

abnormal <- data[data$actual < 0 & data$predicted > 0 & data$ret > 0, ]

normal <- data[data$actual > 0 & data$predicted > 0 & data$ret < 0, ]
nrow(subset(data, ret > 0.05 & predicted > 1))/nrow(subset(data, predicted > 1))
