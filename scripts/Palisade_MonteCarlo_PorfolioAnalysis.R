library(readxl)

SimNum <- 5000

HistPrice <- as.data.frame(read_excel("./data/StockHistoricalPrices.xlsx"))

MonReMean <- colMeans(HistPrice[, 7:11], na.rm = TRUE)
MonReSd <- apply(HistPrice[, 7:11], 2, sd, na.rm = TRUE)

YearReMean <- 12 * MonReMean
YearReSd <- sqrt(12) * MonReSd

CurrentStockPrice <- c(25.47, 33.42, 92.97, 41.33, 37.48)
StockShares <- c(1000, 2000, 1500, 500, 1500)
PutExPrices <-  c(24.00, 32.00, 90.00, 40.00, 36.00)
PutsPurchased <- c(3, 2, 4, 1, 2)
SharePerPut <- c(100, 100, 100, 100, 100)
HoldingTime <- c(0.5, 0.5, 0.5, 0.5, 0.5)
RiskFreeRate <- 0.055

d1 <- (log(CurrentStockPrice/PutExPrices) + (RiskFreeRate+0.5*YearReSd^2)*HoldingTime) / (YearReSd * sqrt(HoldingTime))
d2 <- (log(CurrentStockPrice/PutExPrices) + (RiskFreeRate-0.5*YearReSd^2)*HoldingTime) / (YearReSd * sqrt(HoldingTime))
Pd1 <- pnorm(0-d1)
Pd2 <- pnorm(0-d2)
PricePerPut <- 0 - (CurrentStockPrice*Pd1 - PutExPrices*exp(0-HoldingTime*RiskFreeRate)*Pd2)*100

names(CurrentStockPrice) <- names(MonReMean)
SimTotalReturn <- vector()
SimTotalReturnPuts <- vector()

for (r in 1:SimNum){
  
  StockReturns <- vector()
  StockSoldPrice <- vector()
  for (i in 1:length(YearReMean)){
    iSoldPrice <- CurrentStockPrice[i] * exp(rnorm(1, (YearReMean[i]-0.5*YearReSd[i]^2)*HoldingTime[i], YearReSd[i]*sqrt(HoldingTime[i])))
    StockSoldPrice <- append(StockSoldPrice, iSoldPrice)
    iReturn <- iSoldPrice / CurrentStockPrice[i] - 1
    StockReturns <- append(StockReturns, iReturn)
  }
  PutExValues <- SharePerPut * pmax((PutExPrices-StockSoldPrice), c(0, 0, 0, 0, 0))
  TotalReturn <- (StockSoldPrice %*% StockShares) / (CurrentStockPrice %*% StockShares) - 1
  TotalReturnPuts <- (StockSoldPrice %*% StockShares + PutsPurchased %*% PutExValues) / (CurrentStockPrice %*% StockShares + PutsPurchased %*% PricePerPut) - 1
  SimTotalReturn <- append(SimTotalReturn, TotalReturn)
  SimTotalReturnPuts <- append(SimTotalReturnPuts, TotalReturnPuts)
}