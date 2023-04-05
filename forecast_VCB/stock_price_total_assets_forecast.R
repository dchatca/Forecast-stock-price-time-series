### MAI XUAN BACH - 11200489
### TIME SERIES ASSIGNMENT

# PART I: INDIVIDUAL
# SUB-PART A: FINANCIAL SERIES

# Read data file and attach

# Read library
library("Metrics")
library('urca')
library(forecast)
library(tseries)

# Save into time series variable: total_assets_vcb_test
total_assets_vcb_test <- ts(total_assets_vcb, start = c(2009, 1), frequency = 4)

# Plot
plot.ts(total_assets_vcb_test)

# Create time variable
time <- seq_along(total_assets_vcb_test)
summary(time)
plot(time, total_assets_vcb_test)

# 1. HoltWinters Additive

hw.total_assets_vcb.a = HoltWinters(total_assets_vcb_test, seasonal = "a")
hw.total_assets_vcb.a

# plot real series and predicted series
plot.ts(total_assets_vcb_test)
lines(fitted(hw.total_assets_vcb.a)[,1], col = "red")

# calculate mape, rmse
mape(total_assets_vcb_test,fitted(hw.total_assets_vcb.a))
mape(total_assets_vcb_test[53:56],fitted(hw.total_assets_vcb.a)[53:56])
rmse(total_assets_vcb_test,fitted(hw.total_assets_vcb.a))
rmse(total_assets_vcb_test[53:56],fitted(hw.total_assets_vcb.a)[53:56])

# Prediction for 2023
hw.total_assets_vcb.a.pred <- predict(hw.total_assets_vcb.a, n.ahead = 4)
hw.total_assets_vcb.a.pred

# 2. HoltWinters Multiplicative

hw.total_assets_vcb.m = HoltWinters(total_assets_vcb_test, seasonal = "m")
hw.total_assets_vcb.m

plot.ts(total_assets_vcb_test)
lines(fitted(hw.total_assets_vcb.m)[,1], col = "green")

mape(total_assets_vcb_test,fitted(hw.total_assets_vcb.m))
mape(total_assets_vcb_test[53:56],fitted(hw.total_assets_vcb.m)[53:56])
rmse(total_assets_vcb_test,fitted(hw.total_assets_vcb.m))
rmse(total_assets_vcb_test[53:56],fitted(hw.total_assets_vcb.m)[53:56])

# Prediction for 2023
hw.total_assets_vcb.m.pred <- predict(hw.total_assets_vcb.m, n.ahead = 4)
hw.total_assets_vcb.m.pred


# create quarterly dummy
s1 <- c(rep(c(1,0,0,0), 14))[1:56]				
s2 <- c(rep(c(0,1,0,0), 14))[1:56]
s3 <- c(rep(c(0,0,1,0), 14))[1:56]
s4 <- c(rep(c(0,0,0,1), 14))[1:56]
# 3. Lin - Lin Trend + Seasonality: Addicative

trend_seas.total_assets_vcb.a = lm(total_assets_vcb_test ~ time + s2+s3+s4)
summary(trend_seas.total_assets_vcb.a)

plot.ts(total_assets_vcb_test)
lines(ts(fitted(trend_seas.total_assets_vcb.a),start = c(2009,1), frequency = 4), col = "green")

mape(total_assets_vcb_test,fitted(trend_seas.total_assets_vcb.a))
mape(total_assets_vcb_test[53:56],fitted(trend_seas.total_assets_vcb.a)[53:56])

rmse(total_assets_vcb_test,fitted(trend_seas.total_assets_vcb.a))
rmse(total_assets_vcb_test[53:56],fitted(trend_seas.total_assets_vcb.a)[53:56])

# Prediction for Q4-2022 and 2023
# 2023Q1, t = 57; s2, s3, s4 = 0
10828241 + 57 * 25993627
# 2023Q2, t = 58; s2 = 1
10828241 + 58 * 25993627 + 6681370
# 2023Q3, t = 59; s3 = 
10828241 + 59 * 25993627 +  9062043
# 2023Q4, t = 60; s4 = 1
10828241 + 60 * 25993627 + 47768441

# 4. Lin - Lin Trend + Seasonality: Multiplicative

trend_seas.total_assets_vcb.m = lm(total_assets_vcb_test ~ time + time*s2 + time*s3+ time*s4)
summary(trend_seas.total_assets_vcb.m)

plot.ts(total_assets_vcb_test)
lines(ts(fitted(trend_seas.total_assets_vcb.m),start = c(2009,1), frequency = 4 ), col = "red")

mape(total_assets_vcb_test,fitted(trend_seas.total_assets_vcb.m))
mape(total_assets_vcb_test[53:56],fitted(trend_seas.total_assets_vcb.m)[53:56])

rmse(total_assets_vcb_test,fitted(trend_seas.total_assets_vcb.m))
rmse(total_assets_vcb_test[53:56],fitted(trend_seas.total_assets_vcb.m)[53:56])

# Prediction for 2023
# 2023Q1, t = 57; s2, s3, s4 = 0
60223150 + 57 * 24164186
# 2023Q2, t = 58; s2 = 1
60223150 + 58 * 24164186 + (-25441429) + 1212580 * 58
# 2023Q3, t = 59; s3 = 1
60223150 + 59 * 24164186 + (-48122894) + 2098063 * 59
# 2023Q4, t = 60; s4 = 1
60223150 + 60 * 24164186 + (-66956882) + 4007122 * 60

# 5. Lin lin model

reg_linear_linear = lm(total_assets_vcb_test~time)
summary(reg_linear_linear)

# plot
total_assets_vcb_forecast = ts(fitted(reg_linear_linear), start = c(2009, 1), frequency = 4)
plot.ts(total_assets_vcb_test)
lines(total_assets_vcb_forecast, col = "pink")

mape(total_assets_vcb_test,fitted(reg_linear_linear)) 
mape(total_assets_vcb_test[53:56],fitted(reg_linear_linear)[53:56]) 
rmse(total_assets_vcb_test,fitted(reg_linear_linear)) 
rmse(total_assets_vcb_test[53:56],fitted(reg_linear_linear)[53:56]) 

# Prediction for 2023
# 2023Q1, t = 57
24719578 + 57 * 26063333
# 2023Q2, t = 58
24719578 + 58 * 26063333
# 2023Q3, t = 59
24719578 + 59 * 26063333
# 2023Q4, t = 60
24719578 + 60 * 26063333


# 6. Log linear model
reg_log_linear = lm(log(total_assets_vcb_test)~time)
summary(reg_log_linear)

# plot
total_assets_vcb_forecast = ts(exp(fitted(reg_log_linear)), start = c(2009, 1), frequency = 4)
plot.ts(total_assets_vcb_test)
lines(total_assets_vcb_forecast, col = "blue")

mape(total_assets_vcb_test,exp(fitted(reg_log_linear))) 
mape(total_assets_vcb_test[53:56],exp(fitted(reg_log_linear))[53:56]) 
rmse(total_assets_vcb_test,exp(fitted(reg_log_linear))) 
rmse(total_assets_vcb_test[53:56],exp(fitted(reg_log_linear))[53:56]) 

# Prediction for 2023
# 2023Q1, t = 57
exp(19.21 + 57 * 0.03759)
# 2023Q2, t = 58
exp(19.21 + 58 * 0.03759)
# 2023Q3, t = 59
exp(19.21 + 59 * 0.03759)
# 2023Q4, t = 60
exp(19.21 + 60 * 0.03759)

# 7. Log - Lin Trend + Seasonality: Addicative

log_lin_trend_seas.total_assets_vcb.a = lm(log(total_assets_vcb_test) ~ time + s2+s3+s4)
summary(log_lin_trend_seas.total_assets_vcb.a)

# plot
plot.ts(total_assets_vcb_test)
lines(ts(exp(fitted(log_lin_trend_seas.total_assets_vcb.a)), start = c(2009, 1), frequency = 4), col = "green")


mape(total_assets_vcb_test,exp(fitted(log_lin_trend_seas.total_assets_vcb.a)))
mape(total_assets_vcb_test[53:56],exp(fitted(log_lin_trend_seas.total_assets_vcb.a))[53:56]) 
rmse(total_assets_vcb_test,exp(fitted(log_lin_trend_seas.total_assets_vcb.a)))
rmse(total_assets_vcb_test[53:56],exp(fitted(log_lin_trend_seas.total_assets_vcb.a))[53:56]) 


# Prediction for 2023
# 2023Q1, t = 57; s2, s3, s4 = 0
exp(19.20 + 57 * 0.03751)
# 2023Q2, t = 58; s2 = 1
exp(19.20 + 58 * 0.03751 + 0.006103)
# 2023Q3, t = 59; s3 = 1
exp(19.20 + 59 * 0.03751 + 0.006123)
# 2023Q4, t = 60; s4 = 1
exp(19.20 + 60 * 0.03751 + 0.05362)

# 8. Log - Lin Trend + Seasonality: Multiplicative

log_lin_trend_seas.total_assets_vcb.m = lm(log(total_assets_vcb_test) ~ time + time * s2 + time * s3 + time * s4)
summary(log_lin_trend_seas.total_assets_vcb.m)

# plot
plot.ts(total_assets_vcb_test)
lines(ts(exp(fitted(log_lin_trend_seas.total_assets_vcb.m)), start = c(2009, 1), frequency = 4), col = "orange")


mape(total_assets_vcb_test,exp(fitted(log_lin_trend_seas.total_assets_vcb.m)))
mape(total_assets_vcb_test[53:56],exp(fitted(log_lin_trend_seas.total_assets_vcb.m))[53:56]) 
rmse(total_assets_vcb_test,exp(fitted(log_lin_trend_seas.total_assets_vcb.m)))
rmse(total_assets_vcb_test[53:56],exp(fitted(log_lin_trend_seas.total_assets_vcb.m))[53:56]) 


# Prediction for 2023
# 2023Q1, t = 57; s2, s3, s4 = 0
exp(19.2008673 + 57 * 0.0374296)
# 2023Q2, t = 58; s2 = 1
exp(19.2008673 + 58 * 0.0374296 + (-0.0020873) + 0.0002953 * 58)
# 2023Q3, t = 59; s3 = 1
exp(19.2008673 + 59 * 0.0374296 + (-0.0046134) + 0.0003756 * 59)
# 2023Q4, t = 60; s4 = 1
exp(19.2008673 + 60 * 0.0374296 + 0.0645596 + (-0.0003569) * 60)


#################################################
# SUB-PART B: STOCK PRICE SERIES

# Read data file and attach

## Save and plot
vcb_stock_price = ts(vcb_stock_price, start = c(20210104, 1), frequency = 1)
summary(vcb_stock_price)
plot.ts(vcb_stock_price)

vcb_stock_price.time<-seq_along(vcb_stock_price)
vcb_stock_price.detrend<-resid(lm(vcb_stock_price~vcb_stock_price.time))
plot.ts(vcb_stock_price.detrend)

plot.ts(diff(vcb_stock_price))

# Plot to indicate order: acf -> MA, PACF -> AR
acf(vcb_stock_price)
pacf(vcb_stock_price)

par(mfrow=c(2,1))
acf(diff(vcb_stock_price))
pacf(diff(vcb_stock_price))

acf(diff(diff(vcb_stock_price)))
pacf(diff(diff(vcb_stock_price)))

# select 1st diff, i = 1

# check unit root
# library('urca')
summary(ur.df(vcb_stock_price, type = 'trend'))
summary(ur.df(diff(vcb_stock_price), type = 'trend'))
summary(ur.df(diff(vcb_stock_price), type = 'drift'))

# For loop to choose min AIC model -> ARIMA 3 0 5
library(hash)

h = hash()
for (p in 1:5) {
  for (q in 1:5) {
      # message("Arima ", p, d, q)
      arima_model = Arima(vcb_stock_price, order = c(p,d,q),include.constant = TRUE)
      # summary(arima_model)
      key = paste(as.character(p), as.character(d), as.character(q))
      h[[key]] = arima_model$aic
      #print(arima_model$aic)
  }
}
val <- unlist(as.list(h))  # convert to list and to named vector
names(val[val == min(val)]) # get the p, d, q of the model whose AIC is lowest

h

# library(forecast)
# ARIMA
reg.vcb.arima215<-Arima(vcb_stock_price, order = c(2,1,5),include.constant = TRUE)
summary(reg.vcb.arima215) # choose
reg.vcb.arima215$aic

reg.vcb.arima305<-Arima(vcb_stock_price, order = c(3,0,5),include.constant = TRUE)
summary(reg.vcb.arima305)

# MA(1), MA(2)
reg.vcb.ma1 <- arma(vcb_stock_price, order = c(0, 1))
summary(reg.vcb.ma1)

reg.vcb.ma2 <- arma(vcb_stock_price, order = c(0, 2))
summary(reg.vcb.ma2)


# AR(1), AR(2), AR(3)
reg.vcb.ar1 <- arma(vcb_stock_price, order = c(1, 0))
summary(reg.vcb.ar1)

reg.vcb.ar2 <- arma(vcb_stock_price, order = c(2, 0))
summary(reg.vcb.ar2)

reg.vcb.ar3 <- arma(vcb_stock_price, order = c(3, 0))
summary(reg.vcb.ar3) # best model now
reg.vcb.ar3$aic

# ARMA(1,1)
reg.vcb.arma11 <- arma(vcb_stock_price, order = c(1, 1))
summary(reg.vcb.arma11) # choose
reg.vcb.arma11$aic

# library(tseries)

# forecast next 10 obs for arima215
vcb.forecast.arima215<-forecast(reg.vcb.arima215, h = 10)
vcb.forecast.arima215

print(data.frame(vcb.forecast.arima215)["Point.Forecast"])
data.frame(vcb.forecast.arima215)$Point.Forecast

# library(Metrics)

# calculate RMSE, MAPE
real_vcb_price_10_obs_next = c(82.6, 82.8, 84, 84,86.9, 87.3, 85.1, 84.8, 85.8, 87.3)
pred_vcb_price_10_obs_next = data.frame(vcb.forecast.arima215)$Point.Forecast
rmse(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next)
mape(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next)

# forecast next 10 obs for reg.vcb.ar3
reg.vcb.ar3.model_to_forecast = arima(vcb_stock_price, order = c(3,0,0))
summary(reg.vcb.ar3.model_to_forecast)
vcb.forecast.ar3 <-forecast(reg.vcb.ar3.model_to_forecast, h = 10)
vcb.forecast.ar3

print(data.frame(vcb.forecast.ar3)["Point.Forecast"])
data.frame(vcb.forecast.ar3)$Point.Forecast

# calculate RMSE, MAPE
pred_vcb_price_10_obs_next_ar3 = data.frame(vcb.forecast.ar3)$Point.Forecast
rmse(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next_ar3)
mape(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next_ar3)

# forecast next 10 obs for reg.vcb.arma11
reg.vcb.arma11.model_to_forecast = arima(vcb_stock_price, order = c(1,0,1))
summary(reg.vcb.arma11.model_to_forecast)
vcb.forecast.arma11 <-forecast(reg.vcb.arma11.model_to_forecast, h = 10)
vcb.forecast.arma11

print(data.frame(vcb.forecast.arma11)["Point.Forecast"])
data.frame(vcb.forecast.arma11)$Point.Forecast

# calculate RMSE, MAPE
pred_vcb_price_10_obs_next_arma11 = data.frame(vcb.forecast.arma11)$Point.Forecast
rmse(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next_arma11)
mape(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next_arma11)