library(readxl)

SimNum <- 2000

HistPrice <- as.data.frame(read_excel("./data/StockHistoricalPrices.xlsx"))

MonReMean <- colMeans(HistPrice[, 7:11], na.rm = TRUE)
MonReSd <- apply(HistPrice[, 7:11], 2, sd, na.rm = TRUE)

YearReMean <- 12 * MonReMean
YearReSd <- sqrt(12) * MonReMean

CurrentStockPrice <- c(25.47, 33.42, 92.97, 41.33, 37.48)
StockShares <- c(1000, 2000, 1500, 500, 1500)
HoldingTime <- c(0.5, 0.5, 0.5, 0.5, 0.5)
names(CurrentStockPrice) <- names(MonReMean)
SimTotalReturn <- vector()

for (r in 1:SimNum){
  
  StockReturns <- vector()
  StockSoldPrice <- vector()
  for (i in 1:length(YearReMean)){
    iSoldPrice <- CurrentStockPrice[i] * exp(rnorm(1, (YearReMean[i]-0.5*YearReSd[i]^2)*HoldingTime[i], YearReSd[i]*sqrt(HoldingTime[i])))
    StockSoldPrice <- append(StockSoldPrice, iSoldPrice)
    iReturn <- iSoldPrice / CurrentStockPrice[i] - 1
    StockReturns <- append(StockReturns, iReturn)
  }
  TotalReturn <- (StockSoldPrice %*% StockShares) / (CurrentStockPrice %*% StockShares) - 1
  SimTotalReturn <- append(SimTotalReturn, TotalReturn)
}