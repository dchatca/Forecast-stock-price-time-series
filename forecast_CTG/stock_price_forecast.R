attach(daily_data)
View(daily_data)
ctg <- ts(daily_data, start = c(20210104,1), frequency = 1)
ctg
summary(ctg)

plot.ts(ctg)
plot.ts(diff(ctg))

ctg.time <- seq_along(ctg)
ctg.time

ctg.detrend <- resid(lm(ctg~ctg.time))
plot.ts(ctg.detrend)

library('urca')
summary(ur.df(ctg, type = 'trend'))
summary(ur.df(diff(ctg), type = 'drift'))

library('forecast')
acf(diff(ctg))
pacf(diff(ctg))


#ARIMA
for (p in 1:5){
  for (q in 1:5){
    arima_model = Arima(ctg, order = c(p,1,q), include.constant = TRUE)
    print(summary(arima_model))
  }
}

# lấy AIC thấp nhất =>
reg.ctg.arima215<-Arima(ctg, order = c(2,1,5),include.constant = TRUE)
reg.ctg.arima215
autoplot(reg.ctg.arima215)
checkresiduals(reg.ctg.arima215)

rmse(ctg, fitted(reg.ctg.arima215))
mape(ctg, fitted(reg.ctg.arima215))*100

ctg.forecast.arima215<-forecast(reg.ctg.arima215, h = 10)
ctg.forecast.arima215

ctg.real <- c(28,28.5,28.5,28.6,29.1,28.55,29.1,28.9,29.1,30)
ctg.real

library('Metrics')
data.frame(ctg.forecast.arima215)$Point.Forecast
rmse(data.frame(ctg.forecast.arima215)$Point.Forecast, ctg.real)
mape(data.frame(ctg.forecast.arima215)$Point.Forecast, ctg.real) * 100


#ARMA
for (p in 1:5){
  for (q in 1:5){
    arma_model = arma(ctg, order = c(p,q))
    print(summary(arma_model))
  }
}

reg.arma31 = arma(ctg, order = c(3,1))
summary(reg.arma31)
reg.arma31 <- arima(ctg, order = c(3,0,1))

rmse(ctg, fitted(reg.arma31))
mape(ctg, fitted(reg.arma31))*100

ctg.forecast.arma31 <- forecast(reg.arma31,h=10)
ctg.forecast.arma31
rmse(data.frame(ctg.forecast.arma31)$Point.Forecast, ctg.real)
mape(data.frame(ctg.forecast.arma31)$Point.Forecast, ctg.real) * 100


#MA
for (p in 0:7){
    ma_model = arma(ctg, order = c(0,p))
    print(summary(ma_model))
}
reg.ma3 <- arima(ctg, order = c(0,0,3))
reg.ma3

rmse(ctg, fitted(reg.ma3))
mape(ctg, fitted(reg.ma3))*100

ctg.forecast.ma3 <-forecast(reg.ma3,h=10)
ctg.forecast.ma3
rmse(data.frame(ctg.forecast.ma3)$Point.Forecast, ctg.real)
mape(data.frame(ctg.forecast.ma3)$Point.Forecast, ctg.real) * 100




#AR
for (p in 0:7){
  ar_model = arma(ctg, order = c(p,0))
  print(summary(ar_model))
}
reg.ar3 <- arima(ctg, order = c(3,0,0))
reg.ar3

rmse(ctg, fitted(reg.ar3))
mape(ctg, fitted(reg.ar3))*100

ctg.forecast.ar3 <-forecast(reg.ar3,h=10)
ctg.forecast.ar3
rmse(data.frame(ctg.forecast.ar3)$Point.Forecast, ctg.real)
mape(data.frame(ctg.forecast.ar3)$Point.Forecast, ctg.real) * 100
