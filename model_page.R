library(tidyverse)
library(dplyr) #包括函數: filter() select() mutate() arrange() summarise() group_by()
library(shiny)
library(shinydashboard) #分開寫法需要
library(ggplot2)
library(shinyWidgets) #設定網頁背景顏色等樣式
library(readxl) ##匯入excel需要的涵式庫
library(kableExtra)
library(intervals)
library(lubridate)
library(zoo)

library(xts)
library(dygraphs)


#
#
#


getwd()
setwd("D:/NCTU_NOT_NYCU/Personal_Project/work")
staticEB2 <- as.data.frame(read_excel("static_EB2.xlsx"))

EBday <- readRDS("EB_day_20180101_20210228_aug.rds")
EBD <- data.frame(
  Time = EBday$datetime,
  Usage = EBday$EB
)
######################################電費數據處理
xts_data_daily <- read_rds("money.rds")
#########################################


#列表警示函數
colcal <- function(a){
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

############################################

sidebar <- dashboardSidebar(
  
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
#####################################################
##  左半邊控制列  ########
##########################
#      selectInput("select", label = h3("分析模式"),
#              #list第一個為下拉列表顯示，第二個為連結用tag
#              choices = list(
#                "電表數據" = "graph1", 
#               "峰值分布" = "peak_scatter", 
#               "用電實地調查推測紀錄" = "record",
#               "可預測性(自相關性)" = "self","測試" = "test"), 
#               selected = 1
#             )
#####################################################
body <- dashboardBody(
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

board <- dashboardPage(dashboardHeader(title = "電表數據展示儀表板"),sidebar,body)


ui <- board
server <- function(input,output){
  ##折線圖#########
  graphing <- reactive({
    filter(EBD,
           between(EBD$Time,
                   input$date[1],
                   input$date[2])
    )
  })
  output$graph <- renderPlot({
    ggplot(graphing(),aes(x = Time,y = Usage),main = "電量分析") +
      geom_line(colour='green') + 
      theme(panel.background = element_rect(fill = 'black', colour = 'white')) +
      scale_x_date(date_labels = "%y-%m-%d")
  })
  ################
  
  
  ##峰值分布######
  
  peak_scatter <- reactive({
    filter(EBD,
           between(EBD$Time,
                   input$date[1],
                   input$date[2]),
           Usage > 1250
    )
  })
  output$peak <- renderPlot({
    ggplot(peak_scatter(),aes(x = Time,y = Usage),main = "峰值分布") +
      geom_point(color = "red") +
      geom_hline(yintercept=1500, linetype="dashed", color = "red")
  })
  
  
  
  ################
  
  ##KABLE#########
  
  output$recording <- function(){
    staticEB2 %>%
      kable("html") %>%
      kable_styling("striped",full_width = F) %>%
      column_spec(3:5,bold = T) %>%
      row_spec(c(colcal(staticEB2)),background = "red")
  }
  
  
  ################
  
  ###自相關函數###
  

  ##可根據顯著自相關性的期數，高於虛線多少，在建置ARIMA(自相關性整合移動平均)模型，
  ##將需要相對應數量的MA係數
  #output$for1 <- renderPlot({
  #  acf(EBts,main = "MA參數估計")
  #})
  #output$for2 <- renderPlot({
  #  pacf(EBts,main = "AR參數估計")
  #})
  
  
  ################
  
  
  ####電費圖片###########
  output$expanse <- renderDygraph({
    dygraph(xts_data_daily) %>%
      dyRangeSelector(height = 40)
  })
  
  
  #######################
  ##################################實地勘察數據處理
  ##table(staticEB2$category)
  #sum(table(table(staticEB2$category))) 找出有幾種不同資料
  category <- as.data.frame(cbind(c("assistant_room","classroom","department_office","lab","library","lounge","meeting_room","office"),rbind(0,0,0,0,0,0,0,0)))
  colnames(category) <- c("category","numbers")
  list <- c("assistant_room","classroom","department_office","lab","library","lounge","meeting_room","office")
  
  for(i in 1:length(staticEB2$category)){
    if(is.na(staticEB2$category[i]) == FALSE){
      for(j in 1:length(list)){
        if(staticEB2$category[i] == list[j]){
          category$numbers[j] = as.numeric(category$numbers[j]) + 1
        }
      }
    }
  }
  output$page1 <- renderPlot({
    ggplot(category,aes(x = category,y = numbers,fill = category)) +
    geom_bar(stat = "identity") + 
      geom_text(aes(label = numbers),vjust = 1.6,size = 5.5,color = "white")
  })
  ##################################################
  
}





shinyApp(ui,server)


##############################測試區
windowsFonts(A=windowsFont("標楷體")) 
p <- ggplot(category,aes(x = category,y = numbers,fill = category)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = numbers),vjust = 1.6,size = 5.5,color = "white") +
  ggtitle("空間資訊使用狀況") + 
  theme(plot.title = element_text(hjust = 0.5,face = "bold",family = "A",size = 20))


         
p
####################################

#**from general to specific
#**introduction background
#**[review] reference
#gap(vital)(lack something)(compare)
#goal (new different)

#method
#result
#disscusion



