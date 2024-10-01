setwd("~/Library/Group Containers/UBF8T346G9.OneDriveSyncClientSuite/OneDrive.noindex/OneDrive/R Project")

library(quantmod)
library(ggplot2)
library(tidyverse)
library(tidyquant)

holdings = read.csv("quotes.csv")

#start = today("Europe/Dublin") - 60 #as.Date("2022-02-01")
#end = today("Europe/Dublin") #as.Date("2022-03-29")
tickers = unique(holdings$Symbol) #c("AAPL", "MSFT", "BTC-EUR", "ETH-EUR", "EURUSD=X")
getSymbols(tickers, src = "yahoo") #, from = start, to = end)

lapply(list(tickers), setNames, c("Open", "High", "Low", "Close", "Volume", "Adjusted"))

for (i in 1:length(tickers)){
  s = tickers[i]
  g = get(s)
  colnames(g) = str_remove(colnames(get(tickers[i])), paste(tickers[i],".", sep = ""))
}

# Find tickets in EUR
which(grepl("EUR", tickers, fixed = TRUE) == "FALSE")

tickers[which(grepl("EUR", tickers, fixed = TRUE) == "FALSE")]

AAPL[,"AAPL.Adjusted"] / `EURUSD=X`[,"EURUSD=X.Adjusted"] #AAPL in EUR
MSFT[,"MSFT.Adjusted"] / `EURUSD=X`[,"EURUSD=X.Adjusted"] #MSFT in EUR

ggplot(AAPL, aes(x=Index,y=AAPL.Adjusted)) + geom_line()

bar_plot = function(x){
  ggplot(get(x), aes(x = Index, y = get(paste(x,".Close",sep="")))) +
    geom_candlestick(aes(open = get(paste(x,".Open",sep="")), high = get(paste(x,".High",sep="")), low = get(paste(x,".Low",sep="")), close = get(paste(x,".Close",sep="")))) +
    labs(title = paste(x,"Bar Chart"), y = "Closing Price", x = "") + 
    theme_tq()
}

bar_plot("AAPL")
bar_plot("MSFT")

getSymbols("EURUSD=X",src="yahoo", from = "2007-01-03", to = today("Europe/Dublin"))
EURUSD = `EURUSD=X`
rm(`EURUSD=X`)

AAPL[min(index(AAPL))]
EURUSD[min(index(AAPL))]

AAPLM = merge(subset(AAPL, select=-AAPL.Volume), as.array(EURUSD$`EURUSD=X.Close`))
x = subset(AAPLM, select=-EURUSD.X.Close)
y <- subset(AAPLM, select=EURUSD.X.Close)
AAPLE <- sweep(x, 1, y, "/")
rm(AAPLM, x, y)
colnames(AAPLE) = c("AAPLE.Open", "AAPLE.High", "AAPLE.Low", "AAPLE.Close", "AAPLE.Adjusted")
na.omit(AAPLE)

bar_plot("AAPL")
bar_plot("AAPLE")

strsplit(colnames(AAPL), split = "[.]")
paste(strsplit(colnames(AAPL), split = "[.]")[[1]][1],"E.",strsplit(colnames(AAPL), split = "[.]")[[1]][2], sep="")



