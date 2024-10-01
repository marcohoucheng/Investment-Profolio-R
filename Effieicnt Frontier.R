setwd("~/OneDrive/R Project")

library(reticulate)
library(timeSeries)
library(fPortfolio)

# Function which returns portfolio object

efficient_portfolio = function(tickers, period = "1y", interval = "1d"){
  yf = import("yfinance")
  no_tickers = length(tickers)
  for (i in 1:no_tickers){
    #stk_ticker = yf$Ticker(tickers[i])
    stk = yf$download(  # or pdr.get_data_yahoo(...
        # tickers list or string as well
        tickers = tickers[i],
        
        # use "period" instead of start/end
        # valid periods: 1d,5d,1mo,3mo,6mo,1y,2y,5y,10y,ytd,max
        # (optional, default is '1mo')
        period = period,
        
        # fetch data by interval (including intraday if period < 60 days)
        # valid intervals: 1m,2m,5m,15m,30m,60m,90m,1h,1d,5d,1wk,1mo,3mo
        # (optional, default is '1d')
        interval = interval)
    stk = stk["Adj Close"]
    colnames(stk) = tickers[i]
    if (i == 1){
      stks = stk
    }
    if (i >= 2){
      stks = merge(x = stks, y = stk, by = 0) # Inner join now, all = T for outter join
      row.names(stks) = as.matrix(stks["Row.names"])
      stks = stks[,-1]
    }
  }
  return(stks)
}

stks = efficient_portfolio(c("AAPL", "MSFT", "TSM", "DIS"), period = "10y", interval = "1d")

industry_returns <- as.timeSeries(stks)

longspec <- portfolioSpec()
setNFrontierPoints(longspec)=1000
setTargetRisk(longspec)="CVaR"

effFrontier <- portfolioFrontier(industry_returns,spec=longspec,constraints=c("LongOnly")) # , "minW=c(0,0)","maxW=c(0.8,0.8)"
effFrontier
getNFrontierPoints(effFrontier)

frontierWeights <- getWeights(effFrontier) # get allocations for each instrument for each point on the efficient frontier
colnames(frontierWeights) <- colnames(stks)
frontierWeights

risk_return <- frontierPoints(effFrontier)
risk_return

mvp <- minvariancePortfolio(industry_returns,spec=longspec,constraints=c("LongOnly", "minW=c(0,0)","maxW=c(0.8,0.8)"))
mvp
mvpweights <- getWeights(mvp)
barplot(mvpweights, main="Low Risk Portfolio Weights", xlab="Asset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))

tan=tangencyPortfolio(industry_returns,constraints=c("LongOnly", "minW=c(0,0)","maxW=c(0.8,0.8)"))
tanweights <- getWeights(tan)                  
barplot(tanweights, main="Medium Risk Portfolio Weights", xlab="Asset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))
tan

maxriskweights = frontierWeights[nrow(frontierWeights),]
barplot(maxriskweights, main="High Risk Portfolio Weights", xlab="Asset", ylab="Weight In Portfolio (%)", col=cm.colors(ncol(frontierWeights)+2), legend=colnames(weights))


# plot frontier
#'Options
#'1: Plot Efficient Frontier
#'2: Plot Minimum Variance Portfolio
#'3: Plot Tangency Portfolio
#'4: Plot Risk Returns of Each Asset
#'5: Plot Equal Weights Portfolio
#'6: Plot Two Asset Frontiers (Long)
#'7: Plot Monte Carlo Portfolios
#'8: Plot Sharpe Ratio

plot(effFrontier,c(1,2,3,4,5))
getNFrontierPoints(effFrontier)
tailoredFrontierPlot(effFrontier)
frontierPlot(effFrontier) # , xlim=c(3.7,12),ylim=c(0.5,1.5)
minvariancePoints(effFrontier, col='red')
singleAssetPoints(effFrontier)
sharpeRatioLines(effFrontier)
#legend("topleft", legend=c("Efficient Frontier","Minimum Risk Portfolio", "Sharp Ratio",'High Risk Portfolio'), col=c("black","red", "blue","green"), pch=c(19,19,NA,19),lty=c(NA,NA,3,NA), cex=1)

