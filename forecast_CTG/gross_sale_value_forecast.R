library(readxl)
quarterly_data_fn <- read_excel("Desktop/DSEB/nam3_hk2/time_series/quarterly_data_fn.xls")
View(quarterly_data_fn)   

ctg <- ts(quarterly_data_fn, start = c(52,1), frequency = 4)
ctg

time <- seq_along(gsr)
time

quarter1 <- c(rep(c(1,0,0,0),13))
quarter2 <- c(rep(c(0,1,0,0),13))
quarter3 <- c(rep(c(0,0,1,0),13))
quarter4 <- c(rep(c(0,0,0,1),13))

#giá trị thật 4 quý năm 2023 lên cổ phiếu 68 lấy nốt quý 4 
ctg_2022 <- c(22132345,25058970,27097979,30361859)
ctg_2022

# linear: trend+seasonality
#additive
summary(lm(ctg ~ time+quarter2+quarter3+quarter4))
#rmse full
library(Metrics)
reg1<-lm(gsr~time+quarter2+quarter3+quarter4)
rmse(gsr, fitted(reg1))
#mape full
mape(gsr, fitted(reg1))*100

#RMSE 4 quarter (giá trị dự đoán từ file excel)
rmse(c(22068624,22422856,22605059,22988573), ctg_2022)

# linear: trend+seasonality
#multiply
summary(lm(gsr~time + quarter2*time + quarter3*time + quarter4*time))
#mape
reg2 <- lm(gsr~time + quarter2*time + quarter3*time + quarter4*time)
#rmse full
rmse(gsr, fitted(reg2))
#mape full
mape(gsr, fitted(reg2))*100
#RMSE 4 quarter 
rmse(c(22212949,22376687,22395893,22579489), ctg_2022)

#Holt Winters : add form
hw.gsr.a <- HoltWinters(gsr, seasonal = 'a')
hw.gsr.a
#rmse full
reg3 <- HoltWinters(gsr, seasonal = 'a')
rmse(gsr, fitted(reg3)[, 1])
#mape full
mape(gsr, fitted(reg3)[, 1])*100
#RMSE 4 quarter
rmse(c(21626172,21980734,22083281,22375396), ctg_2022)

#Holt Winters : mul form
hw.gsr.m <- HoltWinters(gsr, seasonal = 'm')
hw.gsr.m
#rmse full
reg4 <- HoltWinters(gsr, seasonal = 'm')
rmse(gsr, fitted(reg4)[, 1])
#mape full
mape(gsr, fitted(reg4)[, 1])*100
#RMSE 4 quarter
rmse(c(21663031,22166139,22220694,22463499), ctg_2022)

#predict for hơlt-winter add
predict(hw.gsr.a,8)

#predict for holt-winter mul
predict(hw.gsr.m,8)

#linear-linear 
reg.ln.ln <- lm(ctg ~ time)
summary(reg.ln.ln)
#rmse
rmse(ctg, fitted(reg.ln.ln))
#mape
mape(ctg, fitted(reg.ln.ln))*100
#RMSE 4 quarter
rmse(c(22063994,22367838,22671682,22975526), ctg_2022)

#linear-log
reg.ln.log <- lm(ctg ~ log(time))
summary(reg.ln.log)
#rmse
rmse(ctg, fitted(reg.ln.log))
#mape
mape(ctg, fitted(reg.ln.log))*100
#RMSE 4 quarter
rmse(c(18614270.38,18703566.53,18791224.13,18877302.22), ctg_2022)

#log - linear
reg.log.ln <- lm(log(gsr) ~ time)
summary(reg.log.ln)
#rmse
rmse(ctg, fitted(reg.log.ln))
#mape
mape(ctg, fitted(reg.log.ln))
#RMSE 4 quarter
rmse(c(24627518.16,25232666.43,25852684.43,26487937.54), ctg_2022)

#log-log
reg.log.log <- lm(log(gsr) ~ log(time))
summary(reg.log.log)
#rmse
rmse(ctg, fitted(reg.log.log))
#mape
mape(ctg, fitted(reg.log.log))
#RMSE 4 quarter
rmse(c(19519115.95,19675281.88,19829797.4,19982709.5), ctg_2022)