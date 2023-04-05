library(readxl)
Daily_data <- read_excel("E:/File Excel/Daily_data.xlsx")
View(Daily_data)

bid <- ts(Daily_data, start = c(20210104,1), frequency = 1)
bid
summary(bid)

plot.ts(bid)
plot.ts(diff(bid))

bid.time <- seq_along(bid)
bid.time

bid.detrend <- resid(lm(bid~bid.time))
plot.ts(bid.detrend)

library('urca')
summary(ur.df(bid, type = 'trend'))
summary(ur.df(diff(bid), type = 'drift'))

library('forecast')
acf(diff(bid))
pacf(diff(bid))


#ARIMA
for (p in 1:5){
  for (q in 1:5){
    arima_model = Arima(bid, order = c(p,1,q), include.constant = TRUE)
    print(summary(arima_model))
  }
}


# lấy AIC thấp nhất =>
reg.bid.arima212<-Arima(bid, order = c(2,1,2),include.constant = TRUE)
reg.bid.arima212
autoplot(reg.bid.arima212)
checkresiduals(reg.bid.arima212)

#RMSE, MAPE
library('Metrics')
rmse(bid, fitted(reg.bid.arima212))
mape(bid, fitted(reg.bid.arima212))*100

#forecast for 10 nearest days of 2023
bid.forecast.arima212<-forecast(reg.bid.arima212, h = 10)
bid.forecast.arima212

#Lấy giá thực 10 ngày đầu của 2023
bid.real <- c(41.2,40.8,40.75,41.65,41,41.3,41.25,41.45,41.8,44.7)
bid.real

#RMSE, MAPE forecast
data.frame(bid.forecast.arima212)$Point.Forecast
rmse(data.frame(bid.forecast.arima212)$Point.Forecast, bid.real)
mape(data.frame(bid.forecast.arima212)$Point.Forecast, bid.real) * 100

#MA
library('tseries')
for (p in 0:7){
  ma_model = arma(bid, order = c(0,p))
  print(summary(ma_model))
}
reg.ma6 <- arima(bid, order = c(0,0,6))
reg.ma6


rmse(bid, fitted(reg.ma6))
mape(bid, fitted(reg.ma6))*100

bid.forecast.ma6 <-forecast(reg.ma6,h=10)
bid.forecast.ma6
rmse(data.frame(bid.forecast.ma6)$Point.Forecast, bid.real)
mape(data.frame(bid.forecast.ma6)$Point.Forecast, bid.real) * 100