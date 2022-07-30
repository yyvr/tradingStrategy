files <- list.files(path='/Users/yang/Downloads/invest/usprices2016/', pattern=NULL, all.files=FALSE,
                    full.names=TRUE)
all <- read.csv('/Users/yang/Downloads/invest/all.csv')

findBuyCandidate <- function(stock, holding) {
  meta <- NULL
  j <- 40
  while(j < (nrow(stock) - interval - jump)) {
    j <- j + jump
    #for (j in seq(40, (nrow(stock) - interval - 1), jump)) {
    head <- j
    tail <- head + interval
    
    if (stock$adjusted[tail]/stock$adjusted[head] > 1.6) next
    
    # price > ema60
    x <- stock$adjusted[head : tail] < EMA(stock$adjusted, 60)[head : tail]
    if (sum(x, na.rm = TRUE) > 1) {
      print(c(1, stock$date[head], stock$date[tail]))
      next
    }
    
    # price < 1.01 * ema20, check price hasn't deviated too much from ema20 
    if (stock$adjusted[tail] >= 1.1 * EMA(stock$adjusted, 20)[tail])
      next
    
    if (EMA(stock$adjusted, 20)[tail] > 1.1 * EMA(stock$adjusted, 60)[tail])
      next
    
    if (EMA(stock$adjusted, 20)[tail] < EMA(stock$adjusted, 30)[tail])
      next
    if (EMA(stock$adjusted, 30)[tail] < EMA(stock$adjusted, 40)[tail])
      next
    if (EMA(stock$adjusted, 40)[tail] < EMA(stock$adjusted, 50)[tail])
      next
    if (EMA(stock$adjusted, 50)[tail] < EMA(stock$adjusted, 60)[tail])
      next
    #print(c(stock$symbol[1], head, tail, EMA(stock$adjusted, 120)[tail],
    #        EMA(stock$adjusted, 120)[head]))
    if (!is.na(EMA(stock$adjusted, 120)[tail]) &&
        !is.na(EMA(stock$adjusted, 120)[head])) {
      if (EMA(stock$adjusted, 120)[tail] < EMA(stock$adjusted, 120)[head])
        next
      if (EMA(stock$adjusted, 60)[tail] < EMA(stock$adjusted, 120)[tail])
        next
      
      if (EMA(stock$adjusted, 60)[head] < EMA(stock$adjusted, 120)[head])
        next
      print(c(2, stock$date[head], stock$date[tail]))
    }
    
    if (stock$adjusted[tail] < EMA(stock$adjusted, 20)[tail])
      next
    
    vol <- max(stock$adjusted[head : tail])/min(stock$adjusted[head : tail])
    if (vol < 1.2) {
      print(c(3, stock$date[head], stock$date[tail]))
      next
    }
    
    # too long rally should avoid buying
    if (tail > 200) {
      x <- EMA(stock$adjusted, 120)[(tail - 200) : tail] < 
        EMA(stock$adjusted, 60)[(tail - 200) : tail]
      #print(c(stock$symbol[1], sum(x, na.rm = TRUE), stock$date[tail]))
      
      if (sum(x, na.rm = TRUE) >= 200) {
        #print(c(stock$symbol[1]), sum(x, na.rm = TRUE))
        print(c(4, stock$date[head], stock$date[tail]))
        #next
      }
    }
    
    sellindex <- tail + holding
    if (sellindex > nrow(stock))
      sellindex <- nrow(stock)
    
    for (k in (tail + 1) : (tail + holding)) {
      if(!is.na(EMA(stock$adjusted, 60)[k]) &&
         stock$adjusted[k] < EMA(stock$adjusted, 60)[k]) {
        sellindex <- k
        break
      }
    }
    
    stock$ret <- c(0, round(diff(stock$adjusted)/stock$adjusted[2 : nrow(stock)], 4))
    
    meta <- rbind(meta, c(stock$symbol[1],
                          stock$date[tail],
                          stock$close[tail],
                          EMA(stock$adjusted, 20)[tail]/EMA(stock$adjusted, 60)[tail],
                          stock$adjusted[tail]/EMA(stock$adjusted, 20)[tail],
                          stock$date[sellindex],
                          stock$close[sellindex],
                          stock$close[sellindex]/stock$close[tail] - 1))
    j <- sellindex + 1
  }
  return (meta)
}