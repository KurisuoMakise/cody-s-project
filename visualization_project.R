library(dplyr)          #包含以下函數:filter() select() mutate() arrange() summarise() group_by()等等

library(shiny) 
library(shinydashboard) #分開寫法
library(shinyWidgets)   #設定網頁背景顏色等外觀

library(ggplot2)        #包含以下函數: ggplot() geom...

library(readxl)         #讀取excel檔案所需函數:read_excel()
library(kableExtra)     #展示實地記錄所使用表格函數:kable() kable_styling()等等

library(dygraphs)       #包含以下函數:dygraph() dyRangeSelector()等等
library(tidyverse)      #包含以下函數:read_rds()

getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/work")

#########################匯入檔案區
staticEB2 <- as.data.frame(read_excel("static_EB2.xlsx"))       #實地空間資訊調查記錄
EBday <- readRDS("EB_day_20180101_20210228_aug.rds")            #工二每日用電紀錄
xts_data_daily <- read_rds("money.rds")                         #電費



#########################數據處理
EBD <- data.frame(
  Time = EBday$datetime,
  Usage = EBday$EB
)

#########################函數創建
colcal <- function(a){                                          #列表警示函數
  m <- mean(a$class_time)
  z <- 0
  li <- c()
  for(i in index(a)){
    if(a$class_time[i] > m){
      li[z+1] <- i
      z = z + 1
    }
  }
  return(li)
}

###############################################網頁側邊(前端)
sidebar <- dashboardSidebar(                                    #選擇欄
  
  sidebarMenu(
    menuItem("控制選單",tabName = "dashboard"),
    dateRangeInput("date",h3("選擇日期(範圍)"),
                   min = min(EBD$Time),
                   max = max(EBD$Time),
                   start = min(EBD$Time),
                   end = max(EBD$Time)
    )
  ),
  selectInput("select", label = h3("分析模式"),
              #list第一個為下拉列表顯示，第二個為連結用tag
              choices = list(
                "電表數據" = "graph1", 
                "峰值分布" = "peak_scatter", 
                "用電實地調查推測紀錄" = "record",
                "電費支出" = "BILL"), 
              selected = 1
  )
  
  
)

###############################################網頁主內容(前端)
body <- dashboardBody(                                         #網頁內容排版
  setBackgroundColor(color = "#2F4F4F"),
  ##第一個condition中input.???，???抓取你要連結的tag
  conditionalPanel(
    condition = "input.select == 'graph1'",
    #HTML的<p></p>意思
    h1(id = "1","電表數據",align = "center"),
    #status->外框顏色
    plotOutput("graph")
  ),
  conditionalPanel(
    condition = "input.select == 'peak_scatter'",
    h1(id = "2","峰值分布",align = "center"),
    plotOutput("peak"),
    hr(),
    fluidRow(
      column(6,plotOutput("page1")),
      column(6,plotOutput("page2"))
    )
  ),
  conditionalPanel(
    condition = "input.select == 'record'",
    h1(id = "3","用電實地調查記錄",align = "center"),
    tableOutput("recording")
  ),
  conditionalPanel(
    condition = "input.select == 'BILL'",
    h1(id = "4","電費支出",align = "center"),
    dygraphOutput("expanse")
  )
)






