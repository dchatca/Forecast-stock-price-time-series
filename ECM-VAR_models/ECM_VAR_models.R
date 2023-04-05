### MAI XUAN BACH - 11200489
### TIME SERIES ASSIGNMENT

# PART II: GROUP


# Read data file and attach

# Read library
library("Metrics")
library('urca')
library(forecast)
library(tseries)
library(lmtest)
library(vars)

# 1. Cointegration, ECM

# Johansen test using “trace” criteria
summary(ca.jo(data.frame(vcb, ctg, bid),type = "trace"))
summary(ca.jo(data.frame(vcb, ctg, bid),type = "eigen"))
 # r = 1 -> have 1 cointegration

# Unit root test for nonstationary series
summary(ur.df(diff(vcb), type = "none"))
summary(ur.df(diff(ctg), type = "none"))
summary(ur.df(diff(bid), type = "none"))
  # these series all ~ I(1)

# Test for Cointegration
# Estimate model
summary(lm(vcb ~ bid + ctg))
summary(lm(bid ~ vcb + ctg))
summary(lm(ctg ~ bid + vcb))

# Stationary Residual test
resid.vcb <- resid(lm(vcb ~ bid + ctg))
summary(ur.df(resid.vcb, type = "none"))

# ECM
summary(lm(diff(vcb) ~ diff(bid) + diff(ctg) + resid.vcb[1:498]))
  # note to comment: coefficient of resid.vcb[1:498] is -0.122009 in range (-1, 0)

# 2. VAR
# Construct variable
d.vcb = diff(vcb)
d.ctg = diff(ctg)
d.bid = diff(bid)
data1 <-data.frame(d.vcb, d.ctg, d.bid)

# Granger causality test
# package “lmtest”
grangertest(d.vcb, d.ctg, order = 1)	
grangertest(d.vcb, d.ctg, order = 2)
  # d.ctg is NOT cause to d.vcb

grangertest(d.vcb, d.bid, order = 1)	
grangertest(d.vcb, d.bid, order = 2)
  # d.bid is NOT cause to d.vcb

##################################
# VAR FOR DIFF SERIES#

# select p in VAR
VARselect(data1, lag.max = 12, type = 'const')$selection
  # p = 1

# VAR(1) model FOR DIFF
# package “vars”
var1 <- VAR(data1, p =1, type = "const")	
summary(var1)

# Serial correlation of Residual test
serial.test(var1)

# Forecast with VAR(1)
forecast1 <-predict(var1)
plot(forecast1)
forecast1
  # forecast1 now: is the forecast for difference only, we have to forecast for the original series
data.frame(forecast1$fcst$d.vcb)$fcst

# Make forecast for vcb series
vcb[499]

diff.vcb.forecast.var = data.frame(forecast1$fcst$d.vcb)$fcst
vcb.forecast.var = c()
calculator = vcb[499] + diff.vcb.forecast.var[1]
vcb.forecast.var = append(vcb.forecast.var, calculator)
for (x in diff.vcb.forecast.var[2:10]) {
  calculator = calculator + x
  vcb.forecast.var = append(vcb.forecast.var, calculator)
}
vcb.forecast.var
rmse(real_vcb_price_10_obs_next, vcb.forecast.var)
mape(real_vcb_price_10_obs_next, vcb.forecast.var)


# Make forecast for ctg series
ctg[499]

diff.ctg.forecast.var = data.frame(forecast1$fcst$d.ctg)$fcst
ctg.forecast.var = c()
calculator = ctg[499] + diff.ctg.forecast.var[1]
ctg.forecast.var = append(ctg.forecast.var, calculator)
for (x in diff.ctg.forecast.var[2:10]) {
  calculator = calculator + x
  ctg.forecast.var = append(ctg.forecast.var, calculator)
}
ctg.forecast.var
rmse(ctg.real, ctg.forecast.var)
mape(ctg.real, ctg.forecast.var)


# Make forecast for bid series
bid[499]

diff.bid.forecast.var = data.frame(forecast1$fcst$d.bid)$fcst
bid.forecast.var = c()
calculator = bid[499] + diff.bid.forecast.var[1]
bid.forecast.var = append(bid.forecast.var, calculator)
for (x in diff.bid.forecast.var[2:10]) {
  calculator = calculator + x
  bid.forecast.var = append(bid.forecast.var, calculator)
}
bid.forecast.var
rmse(bid.real, bid.forecast.var)
mape(bid.real, bid.forecast.var)



# Impulse response function
irf(var1)
plot(irf(var1))
  # note to comment: the covergence

# Forecast error variance decomposition – fevd
fevd(var1)
plot(fevd(var1))

##################################
# VAR FOR SELF SERIES: VCB, CTG, BID#

VARselect(data.frame(vcb, ctg, bid), lag.max = 12, type = 'const')$selection

# VAR(1) model FOR sERIES (var0)
var0 <- VAR(data.frame(vcb, ctg, bid), p=1, type = 'const', season = NULL, exogen = NULL)
summary(var0)

# Serial correlation of Residual test
serial.test(var0)

# Forecast with VAR(1) self series
forecast0 <-predict(var0)
plot(forecast0)
forecast0

# VCB
real_vcb_price_10_obs_next = c(82.6, 82.8, 84, 84,86.9, 87.3, 85.1, 84.8, 85.8, 87.3)
pred_vcb_price_10_obs_next_VAR = data.frame(forecast0$fcst$vcb)$fcst
rmse(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next_VAR)
mape(real_vcb_price_10_obs_next, pred_vcb_price_10_obs_next_VAR)

# CTG
ctg.real <- c(28,28.5,28.5,28.6,29.1,28.55,29.1,28.9,29.1,30)
ctg.pred = data.frame(forecast0$fcst$ctg)$fcst
rmse(ctg.real, ctg.pred)
mape(ctg.real, ctg.pred)

# BID
bid.real <- c(41.2, 40.8, 40.75, 41.65, 41, 41.3, 41.25, 41.45, 41.8, 44.7)
bid.pred = data.frame(forecast0$fcst$bid)$fcst
rmse(bid.real, bid.pred)
mape(bid.real, bid.pred)

# Run ARIMA for BID
library(hash)

h = hash()
for (p in 1:5) {
  for (q in 1:5) {
    # message("Arima ", p, d, q)
    arima_model = Arima(bid, order = c(p,d,q),include.constant = TRUE)
    # summary(arima_model)
    key = paste(as.character(p), as.character(d), as.character(q))
    h[[key]] = arima_model$aic
    #print(arima_model$aic)
  }
}
val <- unlist(as.list(h))  # convert to list and to named vector
names(val[val == min(val)]) # get the p, d, q of the model whose AIC is lowest

h

bid.forecast.arima215<-forecast(Arima(bid, order = c(2,1,5),include.constant = TRUE), h = 10)
bid.forecast.arima215
plot(forecast(bid.vcb.arima215))
print(data.frame(bid.forecast.arima215)["Point.Forecast"])
data.frame(bid.forecast.arima215)$Point.Forecast

rmse(bid.real, data.frame(bid.forecast.arima215)$Point.Forecast)
mape(bid.real, data.frame(bid.forecast.arima215)$Point.Forecast)

# Impulse response function
irf(var0)
plot(irf(var0))

# Forecast error variance decomposition – fevd
fevd(var0)
plot(fevd(var0))
