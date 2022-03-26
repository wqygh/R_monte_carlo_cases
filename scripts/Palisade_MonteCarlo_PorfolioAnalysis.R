library(readxl)
library(MASS)

SimNum <- 5000

HistPrice <- as.data.frame(read_excel("./data/StockHistoricalPrices.xlsx"))

StockN <- ncol(HistPrice[7:11])

MonReMean <- colMeans(HistPrice[, 7:11], na.rm = TRUE)
MonReSd <- apply(HistPrice[, 7:11], 2, sd, na.rm = TRUE)

YearReMean <- 12 * MonReMean
YearReSd <- sqrt(12) * MonReSd

CurrentStockPrice <- c(25.47, 33.42, 92.97, 41.33, 37.48)
StockShares <- c(1000, 2000, 1500, 500, 1500)
PutExPrices <-  c(24.00, 32.00, 90.00, 40.00, 36.00)
PEPMat <- matrix(PutExPrices, nrow=SimNum, ncol=StockN, byrow=TRUE)
PutsPurchased <- c(3, 2, 4, 1, 2)
SharePerPut <- c(100, 100, 100, 100, 100)
HoldingTime <- c(0.5, 0.5, 0.5, 0.5, 0.5)
RiskFreeRate <- 0.055

d1 <- (log(CurrentStockPrice/PutExPrices) + (RiskFreeRate+0.5*YearReSd^2)*HoldingTime) / (YearReSd * sqrt(HoldingTime))
d2 <- (log(CurrentStockPrice/PutExPrices) + (RiskFreeRate-0.5*YearReSd^2)*HoldingTime) / (YearReSd * sqrt(HoldingTime))
Pd1 <- pnorm(0-d1)
Pd2 <- pnorm(0-d2)
PricePerPut <- 0-(CurrentStockPrice*Pd1 - PutExPrices*exp(0-HoldingTime*RiskFreeRate)*Pd2)*100

CorMat <- matrix(0.7, nrow=StockN, ncol=StockN) + diag(StockN)*0.3

SamplingMu <- matrix((YearReMean-0.5*YearReSd^2) * HoldingTime, nrow=SimNum, ncol=StockN, byrow=TRUE)
SamplingSig <- diag(YearReSd * sqrt(HoldingTime))

IndeSamples <- SamplingMu + mvrnorm(SimNum, rep(0, StockN), diag(StockN))%*%SamplingSig
CorrSamples <- SamplingMu + mvrnorm(SimNum, rep(0, StockN), CorMat)%*%SamplingSig

IndeSoldPriceMat <- exp(IndeSamples)%*%diag(CurrentStockPrice)
CorrSoldPriceMat <- exp(CorrSamples)%*%diag(CurrentStockPrice)

PutExValues <- pmax((PEPMat-IndeSoldPriceMat), matrix(0, nrow=SimNum, ncol=StockN)) %*% diag(SharePerPut)

IndeReturns <- (IndeSoldPriceMat%*%StockShares) / as.numeric(CurrentStockPrice%*%StockShares) - 1
CorrReturns <- (CorrSoldPriceMat%*%StockShares) / as.numeric(CurrentStockPrice%*%StockShares) - 1

IndeReturnsPuts <- (IndeSoldPriceMat%*%StockShares + PutExValues%*%PutsPurchased) / as.numeric(CurrentStockPrice%*%StockShares + PricePerPut%*%PutsPurchased) - 1
