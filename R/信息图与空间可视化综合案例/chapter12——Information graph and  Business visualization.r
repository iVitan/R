## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## visualization of geospatial and data map_case and model 

## 《R语言商务图表与数据可视化》
## 课程讲师——杜雨
## 课程机构——天善智能

########第十二章：信息图与空间数据可视化综合案例思路解读######

######一、 北京历史天气######

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

mytable <- read.csv("F:/WorkPlace/R/R/信息图与空间可视化综合案例/beijingtianqi.csv",stringsAsFactors=FALSE,check.names=FALSE) 

#作图数据准备
mydata11  <- mytable %>% 
  select(date,AQI,Year)  %>% 
  arrange(Year,date) %>% #按年、日期排序
  filter(date!="2016-02-29")#为了保证每年数据天数一致删去2016-02-29

mydata11$ID   <- rep(seq(from=1,to=365),3) #每年按日期生成ID

# 极坐标图每一年的半径
mydata11$Asst <- 5
mydata11$Asst[mydata11$Year == 2015] <- 10
mydata11$Asst[mydata11$Year == 2016] <- 15

#月份标签及其旋转角度
mydata11$Month     <- month(mydata11$date)
mydata11$Monthdata <- -5
mydata11$Monthjo   <- ifelse(mydata11$Month %% 2==0,"A","B")#划分单双月

circlemonth        <- seq(15,345,length=12) # 月份的旋转角度
circlebj           <- rep(c(-circlemonth[1:3],rev(circlemonth[1:3])),2) # AQI的旋转角度
circlelabel        <- paste0(1:12,"月") # 月份text

#取月度数据子集
mydata11A <- mydata11[mydata11$Year == 2014 & mydata11$Monthjo == "A",]
mydata11B <- mydata11[mydata11$Year == 2014 & mydata11$Monthjo == "B",]

#1.1 月份圆环图：
ggplot()+
  geom_bar(data=mydata11A,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1") +
  geom_bar(data=mydata11B,aes(x=ID,y=Monthdata),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1") +
  geom_text(data=NULL,aes(x= circlemonth ,y=-2.5,label = circlelabel,angle=circlebj),family="myfont",size=7,hjust=0.5,vjust=.5)+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20)

#取季度数据子集
mydata11$Quarter     <- quarter(mydata11$date)#把数据划分为4分
mydata11$Quarterdata <- 20 #增加一列值为20的Quarterdata列
# 1，3季度的数据
mydata11C <- mydata11 %>% filter(mydata11$Year==2014) %>% filter(Quarter %in% c(1,3)) 
# 2，4季度的数据
mydata11D <- mydata11 %>% filter(mydata11$Year==2014) %>% filter(Quarter %in% c(2,4)) 

# 季度的柱子旋转度数和长度
circlequarter<-seq(45,315,length=4)
# 季度的文字旋转度数
circleqd<-rep(c(-circlequarter[1],circlequarter[1]),2)

#1.2 季度圆环图：
ggplot()+
  geom_bar(data=mydata11C,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#BDBDBD",col="#BDBDBD")+
  geom_bar(data=mydata11D,aes(x=ID,y=Quarterdata),stat="identity",width=1,fill="#D4D2D3",col="#D4D2D3")+
  geom_text(data=NULL,aes(x=circlequarter,y=17.5,label=paste0(c("一","二","三","四"),"季度"),angle=circleqd),family="myfont",size=7,hjust=0.5,vjust=.5)+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20)


#空气质量AQI指标离散化（使用cut函数进行离散等距分箱）

mydata11$FADD<-cut(
  mydata11$AQI,
  breaks=c(0,50,100,150,200,300,500),
  labels=c("0~50","50~100","100~150","150~200","200~300","300~500"),
  order= TRUE
  )

#1.3 主图——天气圆环图：
ggplot()+
  geom_bar(data=mydata11[mydata11$Year==2016,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_bar(data=mydata11[mydata11$Year==2015,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  geom_bar(data=mydata11[mydata11$Year==2014,],aes(x=ID,y=Asst,fill=FADD),stat="identity",width=1)+
  scale_fill_brewer(palette="YlOrRd",direction=1,guide=guide_legend(reverse=TRUE))+
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta="x")+
  ylim(-20,20) +
  guides(colour=guide_legend(reverse=TRUE))+
  annotate("text",x=0,y=-15,label="北京",size=25,hjust=.5,vjust=1,family="myfont")


#1.4 合成最终版图形：
font_add("myfont","msyhl.ttc")
CairoPNG(file="F:/beijinglishitiqi.png",width=1488,height=996)
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

showtext_end()
dev.off()

##
#### 二、ECOcircle  ####
##

#2.1 导入加载包：
rm(list = ls())
gc()
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

ECOdata <- read.xlsx(
  "F:/WorkPlace/R/R/信息图与空间可视化综合案例/ECOCircle.xlsx",
  sheetName="Sheet1",
  header=T,
  encoding="UTF-8",
  stringsAsFactors=FALSE
)

#2.2 使用循环构造作图数据：

transfun <- function(data){
  f <- c(99,91,84,77) #散点图的距离
  for (i in 1:nrow(data)){
    m <- sum(data[i,3:6] == 1) # 找出选举的4列里是1的相加
    h <- grep(1,data[i,3:6]) # 找出选举的4列里是1的
    data[i,h+2]<- f[1:m] # 把是1的按次序替换为f的值
  }
  return(data)
}

ECOdata <- transfun(ECOdata) # 调用函数替换全局的1

#2.3 
# 生成2014年每一天的日期
dateseries <- data.frame(
  newdate = seq(from=as.Date("2014-01-01"),to=as.Date("2014-12-31"),by="1 day")
)

mydata <-dateseries %>% mutate(
  newmonth = newdate %>% month(), # 提取月份
  ID       = 1:nrow(dateseries), # ID 为国家名排序
  Type     = ifelse(newmonth%%2 == 0,"A","B"), # 划分单双月（A双月）
  Scale    = 100,
  Circle   = c(seq(from=90,to=-90,length=181),seq(from=90,to=-90,length=184))
)

# 拼接两个表
mynewdata <- merge(
  mydata,
  ECOdata,
  by.x="newdate",
  by.y="date",
  all.x= T # 保留左表的内容
)

# 转换为宽表格
mynewdataone <- melt(
  na.omit(mynewdata), 
  id.vars =names(mynewdata)[1:7],
  variable.name = "Class", 
  value.name = "Fact"
 )


#月份标签旋转角度：

circlemonth <- seq(15,345,length=12)
circlebj    <- rep(c(-circlemonth[1:3],rev(circlemonth[1:3])),2)

#将事件圆环图的主体部分分为两个图层单独映射（把fill美学标度映射的机会留给后面的散点图，这样散点图可以提供填充色和轮廓色，提升审美水平！）




# 事件圆环图——外环
ggplot()+
  geom_bar(data=mydata[mydata$Type=="A",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
  geom_bar(data=mydata[mydata$Type=="B",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
  ylim(-90,130)+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(limits=c("Legislative","Referendum","President","Primary"),values=c("#93A299","#CF543F","#B5AE53","#86825B"),labels=c("立法","公投","总统","初选"))+
  guides(colour=guide_legend(title=NULL))+
  coord_polar(theta="x")+
  labs(
    title="2014年全球选举事件图",
    subtitle="这是一幅用心良苦的好图",
    caption="Source：Economics\nMake:DataMofang"
  ) +
  theme_void()

# 事件圆环图——散点图
ggplot()+
  geom_point(data=mynewdataone[mynewdataone$Fact!=0,],aes(x=ID,y=Fact,fill = Class),size=5,shape=21,col="white")+
  ylim(-90,130)+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(limits=c("Legislative","Referendum","President","Primary"),values=c("#93A299","#CF543F","#B5AE53","#86825B"),labels=c("立法","公投","总统","初选"))+
  guides(colour=guide_legend(title=NULL))+
  coord_polar(theta="x")+
  labs(
    title="2014年全球选举事件图",
    subtitle="这是一幅用心良苦的好图",
    caption="Source：Economics\nMake:DataMofang"
  ) +
  theme_void()

#事件圆环图——标签：

mynewdataoneA <- mynewdataone %>% filter(Fact!=0 & ID < 180) 
mynewdataoneA <- mynewdataoneA[!duplicated(mynewdataoneA[,1]),]

mynewdataoneB <- mynewdataone %>% filter(Fact!=0 & ID > 180)
mynewdataoneB <- mynewdataoneB[!duplicated(mynewdataoneB[,1]),]

ggplot()+
  geom_text(data=mynewdataoneA,aes(x=ID,y=max(mynewdataoneA$Fact)+20,label=State,angle=Circle),family="sans",size=5,hjust=0,vjust=.5)+
  geom_text(data=mynewdataoneB,aes(x=ID,y=max(mynewdataoneB$Fact)+20,label=State,angle=Circle),family="sans",size=5,hjust=1,vjust=.5)+
  geom_text(data=NULL,aes(x=circlemonth,y=30,label=paste0(1:12,"月"),angle=circlebj),family="myfont",size=7,hjust=.5,vjust=.5)+
  ylim(-90,130)+
  scale_x_continuous(expand = c(0,0))+
  coord_polar(theta="x")+
  labs(
    title="2014年全球选举事件图",
    subtitle="这是一幅用心良苦的好图",
    caption="Source：Economics\nMake:DataMofang"
  ) +
  theme_void()

#图表综合版（不含文本）
ggplot()+
  geom_bar(data=mydata[mydata$Type=="A",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
  geom_bar(data=mydata[mydata$Type=="B",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
  geom_point(data=mynewdataone[mynewdataone$Fact!=0,],aes(x=ID,y=Fact,fill=Class),size=5,shape=21,col="white")+
  ylim(-90,130)+
  scale_x_continuous(expand = c(0,0))+
  scale_fill_manual(limits=c("Legislative","Referendum","President","Primary"),values=c("#93A299","#CF543F","#B5AE53","#86825B"),labels=c("立法","公投","总统","初选"))+
  guides(colour=guide_legend(title=NULL))+
  coord_polar(theta="x")+  # 坐标转换
  theme_void() 


#事件圆环图——汇总版

font_add("myfont","msyhl.ttc")
CairoPNG(file="F:/ECOCircle.png",width=1000,height=1050) # 图片保存路径
showtext_begin()
ggplot()+
  geom_bar(data=mydata[mydata$Type=="A",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#ECEDD1",col="#ECEDD1")+
  geom_bar(data=mydata[mydata$Type=="B",],aes(x=ID,y=Scale),stat="identity",width=1,fill="#DFE0B1",col="#DFE0B1")+
  geom_point(data=mynewdataone[mynewdataone$Fact!=0,],aes(x=ID,y=Fact,fill=Class),size=5,shape=21,col="white")+
  geom_text(data=mynewdataoneA,aes(x=ID,y=max(mynewdataoneA$Fact)+20,label=State,angle=Circle),family="sans",size=5,hjust=0)+
  geom_text(data=mynewdataoneB,aes(x=ID,y=max(mynewdataoneB$Fact)+20,label=State,angle=Circle),family="sans",size=5,hjust=1)+
  geom_text(data=NULL,aes(x=circlemonth,y=30,label=paste0(1:12,"月"),angle=circlebj),family="myfont",size=7,hjust=.5,vjust=.5)+
  ylim(-90,130)+
  scale_x_continuous(expand = c(0,0))+
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



####三、 美国总统大选结果可视化仪表盘 ####

#研究问题：
#1、美国各州选举人票分布状况；
#2、美国总统选举结果最终双方（民主党、共和党）各自获胜状况；
#3、民主党（希拉里）各州选票获取情况；
#4、共和党（特朗普）各州选票获取情况。

#加载包
library("ggplot2")
library("RColorBrewer")
library("ggthemes")
library("shiny")
library("albersusa")
library("shinydashboard")

#导入数据
newdata<-read.csv(
  "F:/R/信息图与空间可视化综合案例/President.csv",
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

# 数据分箱
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


####四、山东省2016年公共预算数据可视化案例####

##因代码量较大、不宜在课件中展示，详情请参考脚本文件shandong&publicBudget.r



