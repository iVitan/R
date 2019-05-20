## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## 北京历史天气

#清空系统内存
rm(list = ls())
gc()

#加载包
library("dplyr")
library("ggplot2")
library("stringr")
library("lubridate")
library("DT")
library("ggplot2")
library("reshape2")
library("RColorBrewer")
library("scales")
library("showtext")
library("grid")
library("Cairo")

mytable <- read.csv("D:/R/File/beijingtianqi.csv",stringsAsFactors=FALSE,check.names=FALSE) 

#作图数据准备
mydata11  <- mytable %>% 
  select(date,AQI,Year)  %>% 
  arrange(Year,date) %>% 
  filter(date!="2016-02-29")

#月份标签及其旋转角度
mydata11$Month     <- month(mydata11$date)
mydata11$Monthdata <- -5
mydata11$Monthjo   <- ifelse(mydata11$Month %% 2==0,"A","B")
circlemonth        <- seq(15,345,length=12)
circlebj           <- rep(c(-circlemonth[1:3],rev(circlemonth[1:3])),2)

#取月度数据子集
mydata11$ID   <- rep(seq(from=1,to=365),3)
mydata11$Year <- factor(mydata11$Year,order=T)
mydata11$Asst <- 5
mydata11$Asst[mydata11$Year==2015] <- 10
mydata11$Asst[mydata11$Year==2016] <- 15
mydata11A<-mydata11[mydata11$Year==2014 & mydata11$Monthjo=="A",]
mydata11B<-mydata11[mydata11$Year==2014 & mydata11$Monthjo=="B",]

#取季度数据子集
mydata11$Quarter     <- quarter(mydata11$date)
mydata11$Quarterdata <- 20
mydata11C<-mydata11 %>% filter(mydata11$Year==2014) %>% filter(Quarter %in% c(1,3)) 
mydata11D<-mydata11 %>% filter(mydata11$Year==2014) %>% filter(Quarter %in% c(2,4)) 
circlequarter<-seq(45,315,length=4)
circleqd <- rep(c(-circlequarter[1],circlequarter[1]),2)

#空气质量AQI指标离散化（使用cut函数进行离散等距分箱）
mydata11$FADD<-cut(
	mydata11$AQI,
    breaks=c(0,50,100,150,200,300,500),
    labels=c("0~50","50~100","100~150","150~200","200~300","300~500"),
    order= TRUE
    )


font_add("myfont","msyhl.ttc")

CairoPNG(file="ECOCircle.png",width=1488,height=996)
showtext.begin()
ggplot()+
  geom_bar(data=mydata11A,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
  geom_bar(data=mydata11B,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
  geom_bar(data=mydata11C,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#BDBDBD",col="#BDBDBD")+
  geom_bar(data=mydata11D,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#D4D2D3",col="#D4D2D3")+
  geom_bar(data=mydata11[mydata11$Year==2016,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_bar(data=mydata11[mydata11$Year==2015,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_bar(data=mydata11[mydata11$Year==2014,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_text(data=NULL,aes(x=circlemonth,y=-2.5,label=paste0(1:12,"月"),angle=circlebj),family="myfont",size=7,hjust=0.5,vjust=.5)+
  geom_text(data=NULL,aes(x=circlequarter,y=17.5,label=paste0(c("一","二","三","四"),"季度"),angle=circleqd),family="myfont",size=7,hjust=0.5,vjust=.5)+
  scale_fill_brewer(palette="YlOrRd",type="seq",direction=1,guide=guide_legend(reverse=TRUE))+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20) +
  guides(colour=guide_legend(reverse=TRUE))+
  annotate("text",x=0,y=-15,label="北京",size=25,hjust=.5,vjust=1,family="myfont") +    
  labs(
    title="2014~2016年度北京市空气质量水平可视化",
    subtitle="数据根据AQI指标水平进行分段分割",
    caption="Source：https://www.aqistudy.cn/",
    x="",y="",fill=""
    )+
  theme_void() %+replace%
  theme(
    panel.border=element_blank(),
    legend.key.size=unit(1.2,'cm'),
    legend.key.height=unit(1,'cm'),
    legend.text.align=1, 
    legend.position=c(1,0),legend.justification=c(1,0),
    legend.text=element_text(size=20,hjust=3,vjust=3,face="bold"),
    plot.title=element_text(size=50,lineheight=1.5,family="myfont"),
    plot.subtitle=element_text(size=35,lineheight=1.5,family="myfont"),
    plot.caption=element_text(size=25,hjust=0,lineheight=1.2,family="myfont"),
    plot.margin=unit(c(.5,.5,.5,.5),"lines")
  )
showtext.end()
dev.off()

###月份圆环及标签：
ggplot()+
  geom_bar(data=mydata11A,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
  geom_bar(data=mydata11B,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
  geom_text(data=NULL,aes(x=circlemonth,y=-2.5,label=paste0(1:12,"月"),angle=circlebj),family="myfont",size=7,hjust=0.5,vjust=.5)+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20) +
  theme_void()


  ###季度圆环及标签：
  ggplot()+
  geom_bar(data=mydata11C,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#BDBDBD",col="#BDBDBD")+
  geom_bar(data=mydata11D,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#D4D2D3",col="#D4D2D3")+
  geom_text(data=NULL,aes(x=circlequarter,y=17.5,label=paste0(c("一","二","三","四"),"季度"),angle=circleqd),family="myfont",size=7,hjust=0.5,vjust=.5)+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20) +
  theme_void() 

  ###中心天气圆环图：
  ggplot()+
  geom_bar(data=mydata11[mydata11$Year==2016,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_bar(data=mydata11[mydata11$Year==2015,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_bar(data=mydata11[mydata11$Year==2014,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  scale_fill_brewer(palette="YlOrRd",type="seq",direction=1,guide=guide_legend(reverse=TRUE))+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20) +
  guides(colour=guide_legend(reverse=TRUE))+
  annotate("text",x=0,y=-15,label="北京",size=25,hjust=.5,vjust=1,family="myfont") +    
  theme_void() 