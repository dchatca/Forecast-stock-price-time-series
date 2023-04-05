library(readxl)
Quarterly_data <- read_excel("E:/File Excel/Quarterly_data.xlsx")
attach(Quarterly_data)
View(Quarterly_data)

bid <- ts(Quarterly_data, start = c(2012,1), frequency = 4)
bid

to_assets <- ts(`Total Assests`, start = c(2012,1), frequency = 4)
to_assets

time <- seq_along(to_assets)
time

#Create dummies
library(tsutils)
seas <- seasdummy(40, 4)
seas

#Real values of 4 quarters in 2020
bid_2022 <- c(1847704434,1979292691,2048952718,2120527692)
bid_2022

#Linear trend + seasonality: additive
summary(lm(to_assets ~ time + seas))
#rmse full
library(Metrics)
reg1<-lm(to_assets~time+seas)
rmse(to_assets, fitted(reg1))
#mape full
mape(to_assets, fitted(reg1))*100
#RMSE 4 quarter (giá trị dự đoán từ file excel)
rmse(c(1749897139,1773271780,1821215139,1845694998), bid_2022)

#Linear trend + seasonality: multi
summary(lm(to_assets~time + time*seas))
#mape
reg2 <- lm(to_assets~time + time*seas)
#rmse full
rmse(to_assets, fitted(reg2))
#mape full
mape(to_assets, fitted(reg2))*100
#RMSE 4 quarter 
rmse(c(1764180763,1727867486,1802369238,1850907024), bid_2022)


#Holt Winters : additive
hw.to_assets.a <- HoltWinters(to_assets, seasonal = 'a')
hw.to_assets.a

#predict for hơlt-winter add
predict(hw.to_assets.a,8)

#rmse full
reg3 <- HoltWinters(to_assets, seasonal = 'a')
rmse(to_assets, fitted(reg3)[, 1])
#mape full
mape(to_assets, fitted(reg3)[, 1])*100
#RMSE 4 quarter
rmse(c(1797020823,1868151399,1911722371,1975719829), bid_2022)

#Holt Winters : multi
hw.to_assets.m <- HoltWinters(to_assets, seasonal = 'm')
hw.to_assets.m

#predict for hơlt-winter mul
predict(hw.to_assets.m,8)

#rmse full
reg4 <- HoltWinters(to_assets, seasonal = 'm')
rmse(to_assets, fitted(reg4)[, 1])
#mape full
mape(to_assets, fitted(reg4)[, 1])*100
#RMSE 4 quarter
rmse(c(1798690201,1873447752,1909769687,1984421160), bid_2022)

#Linear-Linear 
reg.ln.ln <- lm(bid ~ time)
summary(reg.ln.ln)
#rmse
rmse(bid, fitted(reg.ln.ln))
#mape
mape(bid, fitted(reg.ln.ln))*100
#RMSE 4 quarter
rmse(c(1744961494,1780345359,1815729224,1851113089), bid_2022)


#Linear-Log
reg.ln.log <- lm(bid ~ log(time))
summary(reg.ln.log)
#rmse
rmse(bid, fitted(reg.ln.log))
#mape
mape(bid, fitted(reg.ln.log))*100
#RMSE 4 quarter
rmse(c(1422058177,1432207705,1442118398,1451801239), bid_2022)

#log - linear
reg.log.ln <- lm(log(to_assets) ~ time)
summary(reg.log.ln)
#rmse
rmse(bid, fitted(reg.log.ln))
#mape
mape(bid, fitted(reg.log.ln))
#RMSE 4 quarter
rmse(c(2024019049,2102294071,2183596227,2268042587), bid_2022)

#log-log
reg.log.log <- lm(log(to_assets) ~ log(time))
summary(reg.log.log)
#rmse
rmse(bid, fitted(reg.log.log))
#mape
mape(bid, fitted(reg.log.log))
#RMSE 4 quarter
rmse(c(1472080562,1489235785,1506180221,1522921274), bid_2022)

