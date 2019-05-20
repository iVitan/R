## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## visualization of U.S. presidential election 

library("ggplot2")
library("RColorBrewer")
library("ggthemes")
library("shiny")
library("albersusa")
library("shinydashboard")

newdata<-read.csv(
  "D:/R/File/President.csv",
  stringsAsFactors=FALSE,
  check.names=FALSE
  ) 

#获取处理过的标准美国地图数据源:
states <-usa_composite() %>% 
  fortify(us,region="name") 

#将选举数据与美国地图数据合并
American_data<-states %>% 
  merge(newdata,by.x="id",by.y="STATE_NAME")

#提取各州中心经纬度数据：
midpos    <- function(AD1){mean(range(AD1,na.rm=TRUE))} 
centres   <- ddply(American_data,.(STATE_ABBR),colwise(midpos,.(long,lat)))
mynewdata <-join(centres,newdata,type="full")

#1 各州选举人票分布状况：
ggplot()+
  geom_polygon(data=American_data,aes(x=long,y=lat,group=group),colour="grey",fill="white")+
  geom_point(data=mynewdata,aes(x=long,y=lat,size=Count,fill=Count),shape=21,colour="black")+
  scale_size_area(max_size=15)+ 
  scale_fill_gradient(low="white",high="#D73434")+
  coord_map("polyconic") +
  theme_map() %+replace% 
  theme(
    legend.position ="none"
    )


#2 美国总统大选投票结果双方获胜州分布情况:

ggplot(American_data,aes(x=long,y=lat,group=group,fill=Results))+
  geom_polygon(colour="white")+
  scale_fill_manual(values=c("#19609F","#CB1C2A"),labels=c("Hillary", "Trump"))+
  coord_map("polyconic") +
  guides(fill=guide_legend(title=NULL))+ 
  theme_map() %+replace% 
  theme(
    legend.position =c(.5,.9),
    legend.direction="horizontal"
    )


#3 民主党（希拉里）各州选票支持率统计：

qa <- quantile(na.omit(American_data$Clinton), c(0,0.2,0.4,0.6,0.8,1.0))
American_data$Clinton_q <- cut(
  American_data$Clinton,
  qa,
  labels=c("0-20%","20-40%","40-60%","60-80%", "80-100%"),
  include.lowest=TRUE
  )

ggplot(American_data,aes(long,lat,group=group,fill=Clinton_q))+
  geom_polygon(colour="white")+
  scale_fill_brewer(palette="Blues")+
  coord_map("polyconic") +
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+ 
  theme_map() %+replace% 
  theme(
    legend.position = c(0.80,0.35),
    legend.text.align=1
    ) 


#4 共和党（特朗普）各州选票支持率统计：

qb <- quantile(na.omit(American_data$Trump),c(0,0.2,0.4,0.6,0.8,1.0))
American_data$Trump_q <- cut(
  American_data$Trump,
  qb,
  labels=c("0-20%","20-40%","40-60%","60-80%","80-100%"),
  include.lowest = TRUE
  )

ggplot(American_data,aes(long,lat,group=group,fill=Trump_q))+
  geom_polygon(colour="white")+
  scale_fill_brewer(palette="Reds")+
  coord_map("polyconic") +
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+ 
  theme_map() %+replace% 
  theme(
    legend.position = c(0.80,0.05),
    legend.text.align=1
    ) 


####美国总统大选shiny仪表盘：####

# 构建UI
ui <- dashboardPage(
  dashboardHeader(title="Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Electoral Vote",  tabName = "dashboard1",icon =icon("dashboard")),
      menuItem("Trump VS Clinton",tabName = "dashboard2",icon =icon("dashboard")),
      menuItem("Hillary's Vote",  tabName = "dashboard3",icon =icon("dashboard")),
      menuItem("Trump's Vote",    tabName = "dashboard4",icon =icon("dashboard")),
      menuItem("Widgets",         tabName = "widgets",  icon =icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard1",   
              fluidRow(
                box(title="Electoral Vote",plotOutput("plot1", width=1000, height=750),width =12)
              )
      ),
      tabItem(tabName = "dashboard2", 
              fluidRow(
                box(title="Trump VS Clinton",plotOutput("plot2", width=1000, height=750),width =12)
              )
      ),
      tabItem(tabName = "dashboard3",
              fluidRow(
                box(title="Hillary's Vote",plotOutput("plot3", width=1000, height=750),width =12)
              )
      ),
      tabItem(tabName = "dashboard4",
              fluidRow(
                box(title="Trump's Vote",plotOutput("plot4", width=1000, height=750),width =12)
              )
      ),
      tabItem(tabName="widgets",
              fluidRow(
                box(
                  title =h2("About Detials"),
                  h3("In 2016, Donald trump won 290 votes and Hillary Clinton won 228. Donald trump finally won, becoming the 45th President of the United States"),
                  width =12
                  )
              )
      )
    )
  )
)

#2 构建服务端代码：

server <- shinyServer(function(input,output){
  output$plot1 <- renderPlot({
    ggplot()+
      geom_polygon(data=American_data,aes(x=long,y=lat,group=group),colour="grey",fill="white")+
      geom_point(data=mynewdata,aes(x=long,y=lat,size=Count,fill=Count),shape=21,colour="black")+
      scale_size_area(max_size=15)+ 
      scale_fill_gradient(low="white",high="#D73434")+
      coord_map("polyconic") +
      theme_map(base_size =15, base_family = "") %+replace% 
      theme(legend.position ="none")
  })
  output$plot2 <- renderPlot({
    ggplot(American_data,aes(x=long,y=lat,group=group,fill=Results))+
      geom_polygon(colour="white")+
      scale_fill_manual(values=c("#19609F","#CB1C2A"),labels=c("Hillary", "Trump"))+
      coord_map("polyconic") +
      guides(fill=guide_legend(title=NULL))+ 
      theme_map(base_size =15, base_family = "") %+replace% 
      theme(legend.position =c(.5,.9),legend.direction="horizontal")
  })
  output$plot3 <- renderPlot({
    qa<-quantile(na.omit(American_data$Clinton), c(0,0.2,0.4,0.6,0.8,1.0))
    American_data$Clinton_q<-cut(American_data$Clinton,qa,labels=c("0-20%","20-40%","40-60%","60-80%", "80-100%"),include.lowest=TRUE)
    ggplot(American_data,aes(long,lat,group=group,fill=Clinton_q))+
      geom_polygon(colour="white")+
      scale_fill_brewer(palette="Blues")+
      coord_map("polyconic") +
      guides(fill=guide_legend(reverse=TRUE,title=NULL))+ 
      theme_map(base_size = 15, base_family = "") %+replace% 
      theme(legend.position = c(0.80,0.05),legend.text.align=1)
  })
  output$plot4 <- renderPlot({
    qb <- quantile(na.omit(American_data$Trump),c(0,0.2,0.4,0.6,0.8,1.0))
    American_data$Trump_q<-cut(American_data$Trump,qb,labels=c("0-20%","20-40%","40-60%","60-80%","80-100%"),include.lowest = TRUE)
    ggplot(American_data,aes(long,lat,group=group,fill=Trump_q))+
      geom_polygon(colour="white")+
      scale_fill_brewer(palette="Reds")+
      coord_map("polyconic") +
      guides(fill=guide_legend(reverse=TRUE,title=NULL))+ 
      theme_map(base_size = 15, base_family = "") %+replace% 
      theme(legend.position = c(0.80,0.05),legend.text.align=1) 
  })
})

#3 运行仪表盘：
shinyApp(ui, server)


