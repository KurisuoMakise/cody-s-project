library(tidyverse)
library(forecast) #AUTO-ARIMA
library(tseries) #adf kpss test
library(dplyr) # mutate %>%

getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/work")
######################################電費數據處理
xts_data_daily <- read_rds("money.rds")
#########################################
EBday <- readRDS("EB_day_20180101_20210228_aug.rds")
EBD <- data.frame(
  Time = EBday$datetime,
  Usage = EBday$EB
)

#歷史資料
history <- EBD %>%
  filter(Time > "2019-01-01") %>%
  filter(Time <= "2019-12-31")
control <- EBD %>%
  filter(Time > "2020-01-01") %>%
  filter(Time < "2020-12-31")


plot(control)
plot(history)
#資料觀察(先將剛剛的資料轉為時間序列資料)
time_format <- ts(history$Usage,frequency = 7,start=min(history$Time),end = max(history$Time))
#time_format
#disassemble <- decompose(time_format)
#window()
#plot(disassemble)

#observe ->實際樣貌
#trend -> 
#seasonal ->
#random -> observe - trend - seasonal 資料的隨機性

adf.test(time_format)
auto.arima(time_format,stepwise = F,trace = T,stationary = T,ic = c("aic"))
fit <- arima(time_format,order = c(1,0,2),seasonal = list(order = c(2,0,0),period = 7))

#模型檢查
windows()
tsdisplay(residuals(fit),lag.max = 50,main = "殘差")

#預測誤差及檢討空間
p <- forecast(fit,364,lambda = 1)
plot(p)

預測 <- as.data.frame(p)
評估 <- cbind(預測,control)
評估 <- 評估 %>%
  mutate(mae = abs(Usage - 評估$`Point Forecast`)) %>%
  mutate(mape = abs(Usage - 評估$`Point Forecast`)/Usage)
mean(評估$mape)
