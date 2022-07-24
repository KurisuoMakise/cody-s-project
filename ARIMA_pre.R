library(forecast) #AUTO-ARIMA
library(tseries) #adf kpss test
library(dplyr) # mutate %>%
library(xts) #as.xts
library(tidyverse)


getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/work")
######################################電費數據處理
xts_data_daily <- readRDS("money.rds")
#########################################
#EBday <- readRDS("EB_day_20180101_20210228_aug.rds")
#EBD <- data.frame(
#  Time = EBday$datetime,
#  Usage = EBday$EB
#)
###################################處理資料
data <- readRDS("data_daily.rds")
plot(data)
###########################################
#歷史資料
time <- seq(as.Date(min(data$datatime)),length = length(data$datatime),by = "days")
data <- as.xts(data$EB2,order.by = time)
colnames(data) <- c("Usage")
data <- data[-c(1),]

adf.test(data)
#接受H1假設，檢定認為該序列為stationary
history <- subset(data,index(data) <= "2017-01-31")
history <- subset(history,index(history) >= "2017-01-01")
#history <- history[-c(60),]
control <- subset(data,index(data) > "2018-01-01")
control <- subset(control,index(control) <= "2018-01-31")

##################################實驗區
#https://otexts.com/fppcn/seasonal-arima.html


###########################################
#資料觀察(先將剛剛的資料轉為時間序列資料)
time_format <- ts(history$Usage,frequency = 1,start=min(index(history)),end = max(index(history)))
time_format
#time_format
disassemble <- decompose(time_format)

#window()

plot(disassemble)
adf.test(time_format)
acf(time_format)
pacf(time_format)
#observe ->實際樣貌
#trend -> 
#seasonal ->
#random -> observe - trend - seasonal 資料的隨機性


auto.arima(time_format,stepwise = F,trace = T,stationary = T,ic = c("aic"))
fit <- arima(time_format,order = c(1,0,1),seasonal = list(order = c(1,0,1),period = 7))

#模型檢查
#windows()
tsdisplay(residuals(fit),lag.max = 24,main = "殘差")

#預測誤差及檢討空間
p <- forecast(fit,30,lambda = 1)
plot(p)

預測 <- as.data.frame(p)
評估 <- cbind(預測,control)
評估 <- 評估 %>%
  mutate(mae = abs(Usage - 評估$`Point Forecast`)) %>%
  mutate(mape = abs(Usage - 評估$`Point Forecast`)/Usage)
mean(評估$mape)
