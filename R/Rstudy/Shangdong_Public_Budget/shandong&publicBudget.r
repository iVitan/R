## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## visualization of Shandong province budget

rm(list = ls())
gc()

library("plyr")
library("dplyr")
library("ggplot2")      
library("rgdal")   
library("Cairo")        
library("RColorBrewer") 
library("grid")
library("xlsx")
library("showtext")
library("magrittr")
#font_add("myfont", "msyh.ttc")
font.add('SimSun', regular = 'C:/Windows/Fonts/Microsoft/SimSun.ttf')

####一、数据处理阶段####

####1.1 处理县级行政区划素材####

shandong_district_data <- read.csv(
  "G:/WorkPlace/R/R/Rstudy/shandong/District.csv",
  stringsAsFactors=FALSE
  )

#以上导入了山东省细化到县级的行政区划名称(所在市、县)、代码(国标统一编码)、以及中心经纬度
#City Country   Code     long      lat
#潍坊市  安丘市 370784 119.2069 36.42742

####################################################################################
#为了方便案例演示，我已经在shandong_district文件中创建了现成的县级行政区的经纬度数据
#如果你想要自己抓取各个县级行政区划的地址，可以利用以下代码进行操纵！
#shandong_district_data <- unite(shandong_district_data,address,City,Name,sep="")
#shandong_district_data <- transform(
#  shandong_district_data,
#  Address = paste0("山东省",address)
#  ) %>% select(c("Address","address","Code","long","lat"))
#shandong_district_data$Address <-  as.character(shandong_district_data$Address)

#library("jsonlite")
#library("httr")
#source("D:/R/web_city_adress.r",encoding = "utf-8")
#result <- GetJD(shandong_district$Address)
######################################################################################

#以下导入山东省各县级行政单位的预算数据：
shandong_zhibiao_data <- read.csv(
  "G:/WorkPlace/R/R/Rstudy/shandong/shddata2.csv",
  #sheetName="Shandongdata",
  header=T,
  #encoding='UTF-8',
  stringsAsFactors=FALSE
  ) 

###字段含义###
#BudgetScale  —— 按公共财政预算收入规模                   【单位（亿元）】
#BudgetGrowth —— 按公共财政预算收入增长幅度               【单位（%）】
#GDPScale     —— 按GDP规模                                【单位（亿元）】
#GDPGrowth    —— 按GDP增长幅度                            【单位（%）】
#PerGDPGrowth —— 按人均GDP                                【单位（万元）】
######

#  City     Address               Country       BudgetScale BudgetGrowth GDPScale GDPGrowth     PerGDPGrowth
#1 滨州市   山东省滨州市滨城区    滨城区        74.2        8.9          606       8.4          8.6
#  城市名称 所在县全程（方便定位）所在区（县）  预算规模    预算增速     GDP规模   GDP增速      人均GDP增速
 

#将预算指标数据与行政区划数据（含有行政区划坐标点）的数据合并：
shandong_district_data <- merge(
  shandong_district_data,
  shandong_zhibiao_data[,-2],
  by = c("City","Country"),
  all.x = TRUE
  )

#   City Country   Code     long      lat BudgetScale BudgetGrowth GDPScale GDPGrowth PerGDPGrowth
# 滨州市  滨城区 371602 118.0201 37.38484        74.2          8.9      606       8.4          8.6

###读入山东省县级行政区划地图素材：

mymap <- readOGR(
  "G:/WorkPlace/R/R/Rstudy/shandong/map.shp",
  stringsAsFactors= FALSE
  )

xs <- mymap@data %>% 
  rename(Code = name) %>% 
  mutate(ID=row.names(.)) %>% 
  select(c("ID","id"))

shandong_district_map_data <- fortify(mymap) %>%  
  left_join(xs,by = c("id" = "ID")) %>% 
  rename(Code = id.y) %>% arrange(group,order) %>% 
  left_join(shandong_district_data[,c(3,6:10)],by = c("Code"="Code"))

#及时清除不需要的中间缓存表
rm(mymap)
rm(xs)
#释放内存空间

###县级数据汇总：###
#1、shandong_district_data       —— 含行政区划名称、代码、中心经纬度、五个核心预算指标
#2、shandong_district_map_data   —— 含行政区划代码、行政区边界点经纬度数据、以及各区（县）指标

####1.2 处理市级行政区划素材####

#导入山东省市级行政区地图素材
shandong_city_map_data <- readOGR(
  "G:/WorkPlace/R/R/Rstudy/shandong/山东省.json",
  encoding = "utf-8",
  use_iconv = TRUE,
  stringsAsFactors=FALSE
  ) 

#提取素材中的市级行政区划信息
city_data <- shandong_city_map_data@data %>% 
  mutate(id = row.names(.)) %>%
  select(c("id","adcode","name")) %>%
  rename(Code = adcode,City = name) 

#将市级行政区划信息合并入市级边界点经纬度坐标数据中：
shandong_city_map_data <-  fortify(shandong_city_map_data) %>%
  merge(city_data,by = "id",all.x = TRUE)
rm(city_data)

##地级市预算指标：
shandongnewdata <- shandong_zhibiao_data %>% 
  group_by(City) %>% 
  summarize(
    SUM_BudgetScale=sum(BudgetScale),
    SUM_GDPScale=sum(GDPScale)
  )
rm(shandong_zhibiao_data) #后期不再使用，清除掉

#将预算指标合并入各市边界点经纬度数据中
shandong_city_map_data <- merge(
  shandong_city_map_data,
  shandongnewdata,
  by ="City"
  ) 
rm(shandongnewdata)

#导入各地级市行政区划数据：
shandong_city_data <- read.csv(
  "G:/WorkPlace/R/R/Rstudy/shandong/City.csv",
  stringsAsFactors=FALSE
)

###数据盘点：
#现在所有的数据已经整理完毕，我们一共得到了四个有用的数据集：

#市级地图数据——shandong_city_map_data（含城市指标及经纬度边界点数据）
#市级指标数据——shandong_city_data（含城市经纬度）

#县级地图数据——shandong_district_map_data（含17个地级市指标及经纬度）
#县级指标数据——shandong_district_data（含137个行政县指标及经纬度和五个指标数据，GDP规模、GDP增速，人均GDP增速，预算规模、预算增速）。

#因为地级市数据限制，只有两个指标（GDP规模和预算规模），所以，最多只能制作两张地图。
#县级行政区数据比较齐全，这里计划的呈现的维度是GDP规模与GDP增速，GDP规模与人均GDP增速，GDP增速与人均GDP增速。预算规模和预算增速，合计一共四张图。

######1.3 山东省各地级市预算数据可视化######

####1.3.1 各市GDP分布：####

range(shandong_city_map_data$SUM_GDPScale)

shandong_city_map_data <- shandong_city_map_data %>%  
  mutate(
    FA_SUM_GDPScale = cut(
      SUM_GDPScale, 
      breaks=c(0,1500,3000,4500,6000,10000),
      labels=c('0~1500','1500~3000','3000~4500','4500~6000','6000~10000'),
      order=TRUE
      ) 
 )

CairoPNG(file="GDPScale222.png",width=1200,height=640)
showtext_begin()
ggplot()+
  geom_polygon(data=shandong_city_map_data,aes(x=long,y=lat,group=group,fill=FA_SUM_GDPScale),col="white",size=.2)+
  geom_text(data = shandong_city_data,aes(x=long,y=lat,label= City),family="myfont",fontface="plain",size=6)+    
  scale_fill_brewer(palette="Greens")+
  coord_map("polyconic") +
  labs(title="山东省各地级市2016年GDP规模分布",subtitle="单位：（亿元）",caption="数据来源：山东省发展与改革委员会")+       
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+   
  theme_void() %+replace% 
  theme(
    title=element_text(family="myfont",size=18),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18), 
    #plot.caption=element_text(family="myfont",size=18), 
    legend.text.align=1, 
    legend.text=element_text(hjust=-5,size=12),          
    legend.position = c(0.08,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()

#####1.3.2 各市预算收入规模分布：####
range(shandong_city_map_data$SUM_BudgetScale)

ggplot(shandong_city_map_data,aes(SUM_BudgetScale))+
  geom_histogram()

shandong_city_map_data$FA_SUM_BudgetScale <- cut(
  shandong_city_map_data$SUM_BudgetScale, 
  breaks=c(0,150,300,450,600,1005),
  labels=c('0~150','150~300','300~450','450~600','600~       '),
  order=TRUE
 )

CairoPNG(file="BudgetScale.png",width=1200,height=640)
showtext_begin()
ggplot()+
  geom_polygon(data=shandong_city_map_data,aes(x=long,y=lat,group=group,fill=FA_SUM_BudgetScale),col="white",size=.2)+
  geom_text(data=shandong_city_data,aes(x=long,y=lat,label=City),family="myfont",fontface="plain",size=6)+    
  scale_fill_brewer(palette="OrRd")+
  coord_map("polyconic") +
  labs(title="山东省各地级市2016年公共预算收入规模分布",subtitle="单位：（亿元）",caption="数据来源：山东省发展与改革委员会")+       
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+   
  theme_void() %+replace%
  theme(
    title=element_text(family="myfont",size=18,hjust = 0),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18), 
    #plot.caption=element_text(family="myfont",size=18), 
    legend.text.align=1, 
    legend.text=element_text(hjust=-5,size=12),          
    legend.position = c(0.08,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()

#以上是山东省地级市预算指标数据可视化两个维度的全部代码，
#通过2016GDP规模以公共预算支出的规模对比可以发现：
#山东省地区间经济发展水平差异比较大，其中青岛在两个指标上都遥遥领先，
#也是全国为数不多的地级市发展好于省会的城市之一。

######1.4 山东省各区（县）预算数据可视化######

######1.4.1 GDP规模&增速######

#查看下GDP的极差分布：
range(shandong_district_map_data$GDPScale,na.rm = TRUE)

ggplot(shandong_district_map_data,aes(GDPScale))+
  geom_histogram()


shandong_district_map_data$FA_GDPScale <- cut(
  shandong_district_map_data$GDPScale, 
  breaks=c(0,300,600,900,1200,3000),
  labels=c('0~300','300~600','600~900','900~1200','1200~3000'),
  order=TRUE
  ) 
summary(shandong_district_map_data$FA_GDPScale)

text_data <- arrange(shandong_district_data,-GDPScale) %>% 
  select(Country,long,lat,GDPScale) %>% top_n(10,GDPScale)

CairoPNG(file="DistrictGDPScale.png",width=1200,height=640)
showtext_begin()
ggplot()+ 
  geom_polygon(data=shandong_district_map_data,aes(x=long,y=lat,group=group,fill=FA_GDPScale),col="grey60",size=.2)+
  geom_point(data=shandong_district_data,aes(x=long,y=lat,size=GDPGrowth),shape=21,fill="#FB832D",col="#ED7D31",alpha=.6)+ #ED7D31E02939
  geom_text(data=text_data,aes(x=long,y=lat-.1,label=Country),family="myfont",fontface="plain",size=4)+    
  scale_size(range=c(1,8),guide=FALSE)+ 
  scale_fill_brewer(palette="Greens")+
  coord_map("polyconic") +
  labs(
    title="山东省各行政县（包含县级市）2016年GDP规模&增速分布",
    subtitle="单位：（亿元）",
    caption="数据来源：山东省发展与改革委员会"
    )+       
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+   
  theme_void() %+replace%
  theme(
    title=element_text(family="myfont",size=18,hjust = 0),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18,hjust = 0), 
    #plot.caption=element_text(family="myfont",size=18,hjust = 0), 
    legend.text.align=1, 
    legend.text=element_text(hjust=0,size=12),          
    legend.position = c(0.02,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()

######1.4.2 GDP规模&人均GDP增速######

CairoPNG(file="DistrictPerGDPScale.png",width=1200,height=640)
showtext_begin()
ggplot()+ 
  geom_polygon(data=shandong_district_map_data,aes(x=long,y=lat,group=group,fill=FA_GDPScale),col="grey60",size=.2)+
  geom_point(data=shandong_district_data,aes(x=long,y=lat,size=PerGDPGrowth),shape=21,fill="#098154",col="#ED7D31",alpha=.6)+ #ED7D31E02939
  scale_size(range=c(1,8),guide=FALSE)+ 
  scale_fill_brewer(palette="OrRd")+
  coord_map("polyconic") +
  labs(title="山东省各行政县（包含县级市）2016年GDP规模&人均GDP增速分布",subtitle="单位：（亿元）",caption="数据来源：山东省发展与改革委员会")+       
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+   
  geom_text(data=text_data,aes(x=long,y=lat-.1,label=Country),family="myfont",fontface="plain",size=4)+    
  theme_void() %+replace%
  theme(
    title=element_text(family="myfont",size=18,hjust = 0),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18), 
    #plot.caption=element_text(family="myfont",size=18), 
    legend.text.align=1, 
    legend.text=element_text(hjust=-5,size=12),          
    legend.position = c(0.05,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()

#####1.4.3 公共预算收入规模&增速分布#####

#查看各县预算规模的极差分布状况。
summary(shandong_district_map_data$BudgetScale)

#将各县预算收入指标按照自定义区间进行等级分箱
shandong_district_map_data$FA_BudgetScale <- cut(
  shandong_district_map_data$BudgetScale, 
  breaks=c(0,40,80,120,160,240),
  labels=c('0~50','50~100','100~150','150~200','200~250'),
  order=TRUE
  ) 

summary(shandong_district_map_data$FA_BudgetScale)

text1_data <-arrange(shandong_district_data,-BudgetScale) %>% 
  select(Country,long,lat,BudgetScale) %>% 
  top_n(10,BudgetScale)

summary(shandong_district_data$BudgetGrowth)

CairoPNG(file="DistrictBudgetScale.png",width=1200,height=640)
showtext_begin()
ggplot()+ 
  geom_polygon(data=shandong_district_map_data,aes(x=long,y=lat,group=group,fill=FA_BudgetScale),col="grey60",size=.2)+
  geom_point(data=shandong_district_data,aes(x=long,y=lat,size=abs(BudgetGrowth)),shape=21,fill=ifelse(shandong_district_data$BudgetGrowth>0,"#FB832D","#014D64"),col="#ED7D31",alpha=.6)+ 
  geom_text(data=text1_data,aes(x=long,y=lat-.1,label=Country),family="myfont",fontface="plain",size=4)+    
  scale_size(range=c(1,8),guide=FALSE)+ 
  scale_fill_brewer(palette="Greens")+
  coord_map("polyconic") +
  labs(
    title="山东省各行政县（包含县级市）2016年公共预算收入规模&增速分布",
    subtitle="单位：（亿元）",
    caption="数据来源：山东省发展与改革委员会"
    )+       
  guides(fill=guide_legend(reverse=TRUE,title=NULL))+   
  theme_void() %+replace% 
  theme(
    title=element_text(family="myfont",size=18,hjust = 0),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18), 
    #plot.caption=element_text(family="myfont",size=18), 
    legend.text.align=1, 
    legend.text=element_text(hjust=-5,size=12),          
    panel.grid = element_blank(),
    legend.position = c(0.02,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()

#以上三个维度分别展示了山东省各县GDP规模与增速指标、
#GDP规模与人均GDP增速指标、财政公共预算收入与增速指标的分布情况


####1.4.4 各县GDP&公共预算收入规模的分布情况####

CairoPNG(file="DistrictGDP&Budget.png",width=1200,height=640)
showtext_begin()
ggplot()+ 
  geom_polygon(data=shandong_district_map_data,aes(x=long,y=lat,group=group),col="grey60",fill="white",size=.2)+
  geom_point(data=shandong_district_data,aes(x=long,y=lat,size=GDPScale,fill=BudgetScale),shape=21,col="grey60",alpha=.8)+ 
  scale_size_area(max_size = 12)+ 
  scale_fill_gradient2(low="#014D64",mid="grey",high="#C72E29",midpoint=70)+
  coord_map("polyconic") +
  labs(
    title="山东省各行政县（包含县级市）2016年GDP规模&公共预算收入分布",
    subtitle="单位：（亿元）",
    caption="数据来源：山东省发展与改革委员会"
    )+   
  annotate("text", x=114.3, y=36.8, label="● 高（预算）", color= "#C72E29", size=6) +    
  annotate("text", x=114.3, y=36.6, label="● 低（预算）", color= "#014D64", size=6) +    
  theme_void() %+replace%
  theme(
    title=element_text(family="myfont",size=18,hjust = 0),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18), 
    #plot.caption=element_text(family="myfont",size=18),        
    legend.position = c(0.06,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()

####1.4.5 GDP增速&公共预算收入增速####
#：
CairoPNG(file="DistrictPerGDPGrowth&BudgetGrodth.png",width=1200,height=640)
showtext_begin()
ggplot()+ 
  geom_polygon(data=shandong_district_map_data,aes(x=long,y=lat,group=group,fill = BudgetGrowth),col="white",size=.2)+
  geom_point(data=shandong_district_data,aes(x=long,y=lat,size=GDPGrowth),shape=21,fill="#006a8e",col="grey95",alpha = .8)+
  scale_fill_gradient2(low="#088158",mid="grey",high="#b1283a",midpoint = 0)+
  scale_size(range = c(0, 12))+
  coord_map("polyconic") +
  labs(
    title="山东省各行政县（包含县级市）2016年GDP增速&公共预算收入增速分布",
    subtitle="单位：（亿元）",
    caption="数据来源：山东省发展与改革委员会"
    )+   
  annotate("text", x=114.3, y=36.4, label="○ GDP（增速）", color= "#014D64", size=6) + 
  annotate("text", x=114.3, y=36.2,   label="● 预算（预算）", color= "#C72E29", size=6) +    
  theme_void() %+replace%
  theme(
    title=element_text(family="myfont",size=18,hjust = 0),
    plot.title=element_text(size=24), 
    plot.subtitle=element_text(family="myfont",size=18), 
    #plot.caption=element_text(family="myfont",size=18),        
    legend.position = c(0.06,0.8),
    plot.caption=element_text(hjust=0)
  )
showtext_end()
dev.off()
####第12章结束######
##################################################################

