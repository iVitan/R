## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## ECOcircle

#1.1 导入加载包：

library("ggplot2")
library("Cairo")
library("xlsx")
library("lubridate")
library("reshape2")
library("showtext")
library("grid")
library("plyr")
library("dplyr")
library("magrittr")

ECOdata<-read.xlsx(
  "D:/R/File/ECOCircle.xlsx",
  sheetName="Sheet1",
  header=T,
  encoding="UTF-8",
  stringsAsFactors=FALSE
  )

#1.2 使用循环构造作图数据：

transfun <- function(data){
  f <- c(99,91,84,77)
  for (i in 1:nrow(data)){
    m <- sum(data[i,3:6] == 1)
    h <- grep(1,data[i,3:6])
    data[i,h+2]<- f[1:m]
  }
  return(data)
}

ECOdata <- transfun(ECOdata)

#1.3 
dateseries <- data.frame(
  newdate = seq(from=as.Date("2014-01-01"),to=as.Date("2014-12-31"),by="1 day")
 )
mydata <-dateseries %>% mutate(
  newmonth = newdate %>% month(),
  ID       = 1:nrow(dateseries),
  Type     = ifelse(newmonth%%2 == 0,"A","B"),
  Scale    = 100,
  Circle   = c(seq(from=90,to=-90,length=181),seq(from=90,to=-90,length=184))
)

mynewdata <- merge(
  mydata,
  ECOdata,
  by.x="newdate",
  by.y="date",
  all.x=T
  )

mynewdataone <- melt(
  na.omit(mynewdata), 
  id.vars =names(mynewdata)[1:7],
  variable.name = "Class", 
  value.name = "Fact"
  )

circlemonth <- seq(15,345,length=12)
circlebj    <- rep(c(-circlemonth[1:3],rev(circlemonth[1:3])),2)

mynewdataoneA<-mynewdataone[mynewdataone$Fact!=0&mynewdataone$ID<180,]
mynewdataoneA<-mynewdataoneA[!duplicated(mynewdataoneA[,1]),]
mynewdataoneB<-mynewdataone[mynewdataone$Fact!=0&mynewdataone$ID>180,]
mynewdataoneB<-mynewdataoneB[!duplicated(mynewdataoneB[,1]),]


font_add("myfont","msyhl.ttc")
CairoPNG(file="ECOCircle.png",width=1000,height=1050)
showtext_begin()
ggplot()+
  geom_bar(data=mydata[mydata$Type=="A",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
  geom_bar(data=mydata[mydata$Type=="B",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
  geom_point(data=mynewdataone[mynewdataone$Fact!=0,],aes(x=ID,y=Fact,fill=Class),size=5,shape=21,col="white")+
  geom_text(data=mynewdataoneA,aes(x=ID,y=max(mynewdataoneA$Fact)+20,label=State,angle=Circle),family="sans",size=5,hjust=0)+
  geom_text(data=mynewdataoneB,aes(x=ID,y=max(mynewdataoneB$Fact)+20,label=State,angle=Circle),family="sans",size=5,hjust=1)+
  geom_text(data=NULL,aes(x=circlemonth,y=30,label=paste0(1:12,"月"),angle=circlebj),family="myfont",size=7,hjust=.5,vjust=.5)+
  ylim(-90,130)+
  scale_fill_manual(limits=c("Legislative","Referendum","President","Primary"),values=c("#93A299","#CF543F","#B5AE53","#86825B"),labels=c("立法","公投","总统","初选"))+
  guides(colour=guide_legend(title=NULL))+
  coord_polar(theta="x")+
  labs(
    title="2014年全球选举事件图",
    subtitle="这是一幅用心良苦的好图",
    caption="Source：Economics\nMake:DataMofang"
    ) +
  theme_void() %+replace%
  theme(
    legend.position=c(0.95,0.92),
    legend.text=element_text(size=20,hjust=0,face="bold",family="myfont"),
    plot.title=element_text(size=50,family="myfont"),
    plot.subtitle=element_text(size=35,family="myfont"),
    plot.caption=element_text(size=25,hjust=0,family="myfont"),
    plot.margin=unit(c(.5,.5,.5,.5),"lines")
  )
showtext_end()
dev.off()


