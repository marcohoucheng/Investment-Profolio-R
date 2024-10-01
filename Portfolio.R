setwd("~/OneDrive/R Project")

library(reticulate)
# py_install("pandas")
# py_install("yfinance", pip = T)

## Enter python
# reticulate::repl_python()
# quit

# Choose stock here
stock = "AAPL"

yf = import("yfinance")
stk = yf$Ticker(stock)

# get historical market data
stk_price = stk$history(period="max")

dates = as.Date(rownames(stk_price))

# Extract weekly prices
weekday_to_num = function(day){
  if (day == "Monday") {return(1)}
  if (day == "Tuesday") {return(2)}
  if (day == "Wednesday") {return(3)}
  if (day == "Thursday") {return(4)}
  if (day == "Friday") {return(5)}
  if (day == "Saturday") {return(6)}
  if (day == "Sunday") {return(7)}
  else {print("Error: Not a day")}
}
weekdays_order = function(arr){
  order = array(NA, length(arr))
  for (i in 1:length(arr)){
    order[i] = weekday_to_num(arr[i])
  }
  return(order)
}

weekdays(dates)
weekdays_order(weekdays(dates))

week_end = which(weekdays_order(weekdays(dates))[-1] <= weekdays_order(weekdays(dates))[-length(weekdays_order(weekdays(dates)))])
if (tail(weekdays(dates),1) == "Friday") {week_end = c(week_end, nrow(dates))}
week_start = week_end + 1
week_start = head(week_start, -1)
week_end = week_end[-1]

stk_price_wk = matrix(NA, nrow = length(week_end), ncol = ncol(stk_price))
colnames(stk_price_wk) = colnames(stk_price)

for (i in 1:length(week_end)){ # from 1 to 2097
  stk_price_wk[i, "Open"] = stk_price[week_start[i], "Open"]
  stk_price_wk[i, "Close"] = stk_price[week_end[i], "Close"]
  stk_price_wk[i, "High"] = max(stk_price[week_start[i]:week_end[i], "High"])
  stk_price_wk[i, "Low"] = min(stk_price[week_start[i]:week_end[i], "Low"])
  stk_price_wk[i, "Volume"] = sum(stk_price[week_start[i]:week_end[i], "Volume"])
  stk_price_wk[i, "Dividends"] = sum(stk_price[week_start[i]:week_end[i], "Dividends"])
  stk_price_wk[i, "Stock Splits"] = sum(stk_price[week_start[i]:week_end[i], "Stock Splits"])
}
rownames(stk_price_wk) = row.names(stk_price[week_start,])
stk_price_wk = data.frame(stk_price_wk)

for (i in 2:length(week_end)){
  k = as.numeric(difftime(rownames(stk_price[week_end[i], ]), rownames(stk_price[week_start[i], ]), units = "days"))
  if (k <= 5){
  }
  else {print("Error:more than 5 days for end of week of")
    print(rownames(stk_price[week_start[i], ]))}
}

install.packages("ggplot2")
library(ggplot2)
stk_price["date"] = as.Date(rownames(stk_price))

ggplot(stk_price, aes(x = as.Date(rownames(stk_price)), y = Close)) + 
  geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) +
  geom_ma(color = "darkgreen") +
  coord_x_date(xlim = c("2010-01-01", "2020-12-31"))
geom_point()

width = 1
ggplot(stk_price, aes(x=as.Date(rownames(stk_price))))+
  geom_linerange(aes(ymin=Low, ymax=High)) +
  theme_bw() +
  labs(title="AAPL") +
  geom_rect(aes(xmin = as.Date(rownames(stk_price)) - width/2 * 0.9, xmax = as.Date(rownames(stk_price)) + width/2 * 0.9, ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = chg)) + guides(fill = FALSE, colour = FALSE) + scale_fill_manual(values = c("dn" = "darkred", "up" = "darkgreen"))



# Plots
plot(dates, stk_price[,1], type = 'l', xlab = "Time", ylab = "$")

MA = function(data, days, t){ # t = "Open", "High", "Low", "Close"
  mov_avg = array(NA, nrow(data)-days+1)
  for (i in 1:(nrow(data)-days+1)){
    mov_avg[i] = mean(data[i:(i+days-1), t])
  }
  return(mov_avg)
}

mov_avg_days = 100

MA(stk_price, mov_avg_days, "Close")
lines(dates[-(1:(mov_avg_days-1))], MA(stk_price, mov_avg_days, "Close"), col = "blue")

?getSymbols
