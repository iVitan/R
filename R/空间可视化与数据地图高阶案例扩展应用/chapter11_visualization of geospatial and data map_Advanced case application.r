## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## visualization of geospatial and data map_case and model 

## 《R语言商务图表与数据可视化》


########第十一章：空间可视化与数据地图高阶案例扩展应用######

####11.1 地图上的mini条形图（柱形图）####
rm(list = ls())
gc()

library("ggplot2")
library("plyr")
library("rgdal")
library("magrittr")
library("dplyr")

china_shp <-readOGR("F:/R/Rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)    

#地图数据（多边形经纬度边界点数据）
china_map <- fortify(china_shp)  

#城市经纬度数据
province_city <- read.csv("F:/R/Rstudy/chinaprovincecity.csv",stringsAsFactors = FALSE) 
province_city1 <- mutate(  #在读进来的省会数据中添加N15、N16即2015和2016年的业务数据，并设置Ratio为表示这两年业务指标上升还是下降
  province_city,
  N15=runif(34,min=500,max=1000),
  N16=runif(34,600,1100),
  Ratio=round((N16-N15)/N15,3)#求解Ratio，并进行四舍五入，保留3位小数
  )
province_data <- province_city1[sample(nrow(province_city1),10),]#从所有省会数据中抽10个样本

#案例一：mini柱形图——竖向
windowsFonts(myFont = windowsFont("微软雅黑")) 
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group = group), fill="white", colour="grey60") +
  geom_linerange(data=province_data,aes(x=jd - 0.5,ymin = wd,ymax = wd + 0.7*N15/max(N15,N16)*5),size=3,color="#5B88A0",alpha=0.8)+#添加竖状条形图，根据图片的刻度范围设置柱子的高度
  geom_linerange(data=province_data,aes(x=jd + 0.5,ymin = wd,ymax = wd + 0.7*N16/max(N15,N16)*5),size=3,color="#FB882C",alpha=0.8)+#添加竖状条形图，根据图片的刻度范围设置柱子的高度
  geom_text(data=province_data,aes(x=jd,y=wd - 0.6,label=paste0(province_data$province,ifelse(Ratio > 0,"▲","▼"),Ratio*100,"%")), family="myFont",size=2.5)+#将省的名字+指标上升下降+%这三部分文字拼接成为geom_text层的文字标签
  # coord_map("polyconic") +
  annotate("text", x=105, y=52, label="● 2015", color= "#5B88A0", size=8)+  #增加文字注释，文字的位置为x=105, y=52
  annotate("text", x=105, y=49, label="● 2016", color= "#FB882C", size=8)+  #增加文字注释，文字的位置为x=105, y=49
  theme_void()


#案例二：mini条形图——纵向条形图
ggplot()+
  geom_polygon(aes(x=long, y=lat,group=group),data=china_map, fill="white", colour="grey60")+
  geom_errorbarh(data=province_data,aes(y=wd,xmin=jd-3,xmax=jd + 2.5*N15/max(N15,N16)),size=3,color="#5B88A0",height=0,alpha=0.8)+#添加横向条形图，根据图片的刻度范围设置柱子的高度
  geom_errorbarh(data=province_data,aes(y=wd-0.8,xmin=jd-3,xmax=jd + 2.5*N16/max(N15,N16)),size=3,color="#FB882C",height=0,alpha=0.8)+#添加横向条形图，根据图片的刻度范围设置柱子的高度
  geom_text(data=province_data,aes(x=jd+0.2,y=wd + 1,label=paste0(province_data$province,ifelse(Ratio>0,"▲","▼"),Ratio*100,"%")), family="myFont",size=2.5)+
  annotate("text", x=105, y=52, label="● 2015", color= "#5B88A0", size=7)+ 
  annotate("text", x=105, y=50, label="● 2016", color= "#FB882C", size=7)+
  coord_equal()+
  theme_void()


####11.2 地图上的mini气泡饼图####
rm(list = ls())
gc()

library("ggplot2")
library("plyr")
library("rgdal")
library("xlsx")
library("ggthemes")
library("magrittr")
library("scatterpie")



#案例：
rm(list = ls())
gc()

china_map <- readOGR(
  "F:/R/Rstudy/china.geojson",
  stringsAsFactors=FALSE,
  encoding = "utf-8",
  use_iconv = TRUE
  )
china_map <- fortify(china_map) 

province_city <- read.csv(
  "F:/R/Rstudy/chinaprovincecity.csv",
  stringsAsFactors = FALSE,
  check.names=FALSE
  )

#构造气泡饼图的指标数据（行政区域-年份-业务指标，1:N:1的关系）
city_data<-data.frame(Name=rep(c("北京","上海","重庆","武汉","广州","西安")))
for (i in 2:7) city_data[,i]<-round(runif(6,0,250))#对于city_data从第2-6列数据，每列数据产生一组均匀分布的随机数
names(city_data)[2:7]<-paste0("Year",2011:2016)#对第2-6列数据的列名设置为Year+对应数字

city_data$Full<-apply(city_data[,-1],1,sum)#按每个地点对各年业务数据求和，并成为city_data的full列
city_data$Full_scale <- scale(city_data$Full,center=F,scale= T )*2#对Full列数据进行中心标准化处理（用于制作饼图时的半径长度）

#提取中心城市数据：

city_data <- city_data %>% merge(province_city[,c("city","wd","jd")],by.x="Name",by.y="city",all.x=TRUE)


ggplot() +    
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",color="grey") +
  geom_scatterpie(data=city_data,aes(x=jd,y=wd,r = Full_scale),cols = names(city_data)[2:7],color="grey", alpha=.8) +#绘制气泡饼图,半径的长短由Full_scale控制，饼图饼块大小由各年的业务数据设置
  coord_equal() +
  scale_fill_brewer(guide=FALSE)+   
  theme_void()
#思考：绘制气泡饼图的关键是确定哪些数据？
#案例扩展——空间地图分面（行政区域—年份-业务指标 1:N:N）
#问：图片想按什么类别呈现数据？按年份还是行政指标还是行政区域？可以按行政区域吗？

city_data2 <- data.frame(Name=rep(city_data$Name,6))
for (i in 2:4) city_data2[,i]<-runif(nrow(city_data2),10,100)#构造三列业务指标数据
names(city_data2)[2:4]<-paste0("Value",1:3)
city_data2$Year<-rep(paste0("Year",2011:2016),each=6)#构造年份列数据

city_data2 <- city_data2 %>% merge(city_data[,c("Name","jd","wd")],by="Name",all.x=T)#将地理位置数据与业务指标拼接
city_data2$Full <- apply(city_data2[,2:4],1,sum) %>% scale(center=F,scale=T)#Full列为每个区域业务指标数据标准化
city_data2$Full <- as.numeric(city_data2$Full)*2#将以上结果标准化后转换成数值型数据并乘以2作为饼图的半径大小
city_data2 <- city_data2%>%arrange(Year,Name)#将city_data2按Year、Name列进行排序

ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",color="grey")+
  geom_scatterpie(data=city_data2,aes(x=jd,y=wd,r=Full),cols = names(city_data2)[2:4],color="grey", alpha=.8) +#气泡饼图的饼块大小由每年3指标值来确定
  coord_equal()+#等坐标转换：使用这个函数后，x轴和y轴坐标会被转换成相等形式，此时图形会产生较大的缩放
  scale_fill_brewer(guide=FALSE)+   
  facet_wrap(~Year) +
  theme_void()

####11.3 地图+网络流向图案例用应（含多种流向类型）####
rm(list = ls())
gc()


library("plyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("maps")
library('devtools')
library("REmap")#关于REmap的安装方法，参考https://blog.csdn.net/BEYONDMA/article/details/85345831，(github搜索包可以进入https://github.com/YTLogos/REmap)
# 安装参考命令devtools::install_local("D://geo_data//REmap-master.zip")#把下载的包的路径写在这里
library("Cairo")
library("baidumap")
#devtools::install_github('heike/ggmapr')
#baidumap安装方法
# library(devtools)
# install_github('badbye/baidumap')
#baidumap加载使用
library(baidumap)
options(baidumap.key ='fengzhiMaMa')#其中，wyky365为个人在百度地图平台申请的账号名http://lbsyun.baidu.com（baidumap库下载地址https://github.com/badbye/baidumap）
library("showtext")


#案例1.1 放射状路径图
china_map <-readOGR("F:/R/Rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)
x <- china_map@data  %>% mutate(id = row.names(.)) 
china_map1 <- fortify(china_map)  
china_map_data <- merge(china_map1,x, by = "id",all.x = TRUE) 
mydata1 <- read.csv("F:/R/Rstudy/geshengzhibiao.csv",stringsAsFactors = FALSE,check.names=FALSE)
china_data <- join(china_map_data, mydata1, type="full") 

#下面百度地图API接口获取指定城市的位置，思考：为什么不用china_map_data里面的long与lat数据
city_list <- c("西安","西宁","郑州","重庆","成都","石家庄","兰州","济南","大同","咸阳","包头","长沙")
source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")#source是执行r脚本文件,web_city_adress.r是在百度Api接口抓取指定地市经纬度的脚本程序
address <- GetJD(city_list)

address$lonx <- address[address$address =="长沙","lng"]#增加列：长沙的经度做为起点的x坐标
address$laty <- address[address$address =="长沙","lat"]#增加列：长沙的纬度做为起点的y坐标
address <- address[address$address!= "长沙",]#将address列中长沙的数据去除（长沙不能做为终点）
names(address)[2:3]<-c("lon","lat")
address$Num <- round(runif(11,50,100),2)#在address中产生Num列，用来控制散点的大小

ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group),fill="white",size=0.2,colour="#D9D9D9")+#增加中国版图层
  geom_segment(data=address,aes(x=lon,y=lat,xend=lonx,yend=laty),size=0.3,colour="#FF6833")+#增加直线段图层
  geom_point(data=address,aes(x=lon,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+#增加散点图层
  geom_point(data=NULL,aes(x=112.97935,y=28.21347),shape=21,size=8,fill=NA,col="steelblue")+#增加长沙所在城市的散点
  guides(fill=FALSE)+
  coord_map("polyconic") +
  scale_size_area(max_size = 8)+ 
  theme_void() %+replace%
  theme(
    plot.background=element_rect(fill="#D0DEDE", color=NA),
    panel.spacing = unit(0,"lines"), 
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position="none"
  )

#案例2：迁徙路径气泡图

city_list <- c("海口","广州","长沙","武汉","郑州","石家庄","北京","沈阳","长春","哈尔滨")
source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")#source是执行r脚本文件,web_city_adress.r是在百度Api接口抓取指定地市经纬度的脚本程序
address <- GetJD(city_list)#调用GetJD函数获取city_list列表指定的城市地理位置
address$Num<-round(runif(10,50,100),2)#address数据框增加Num列代表指标值大小

ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group),fill="white",size=0.2,colour="#D9D9D9")+
  geom_path(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+#增加路径图（按address城市先后顺序自动绘制路径）
  geom_point(data=address,aes(x=lng,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+#增加散点图控制显示各城市相关指标值
  guides(fill=FALSE)+
  coord_map("polyconic")+
  scale_size_area(max_size=8)+ 
  theme_void() %+replace%
  theme(
    plot.background=element_rect(fill="#D0DEDE", color=NA),
    panel.spacing = unit(0,"lines"), 
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position="none"
  )

#案例1.3：闭环路径气泡图：思考一下，如何调整迁徙路径成为闭环路径

city_list <- c("兰州","成都","重庆","贵阳","昆明","南宁","海口","广州","福州","上海","青岛","石家庄","呼和浩特","银川")
city_list <- c(city_list,city_list[1])#城市列表里增加兰州作为路径终点城市

source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(nrow(address),50,100),2)

ggplot()+
  geom_polygon(data=china_data,aes(x=long,y=lat,group=group),fill="white",size=0.2,colour="#D9D9D9")+#绘制散点图
  geom_path(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+#绘制路径图
  geom_point(data=address,aes(x=lng,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+#绘制散点图
  guides(fill=FALSE)+
  coord_map("polyconic")+
  scale_size_area(max_size=8)+ 
  theme_void() %+replace%
  theme(
    plot.background=element_rect(fill="#D0DEDE", color=NA),
    panel.spacing = unit(0,"lines"), 
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position="none"
  )
#案例2：多维度放射状(多维度？显示地点多个，每个地点有多个指标数据)

library("ggplot2")
library("dplyr")
library("rgdal")
library("shiny")
library("shinythemes")
library("magrittr")
rm(list = ls())
gc()

##转换为数据框并合并城市数据：
china_map <- readOGR(
  "F:/R/Rstudy/china.geojson",
  stringsAsFactors=FALSE,
  encoding = "utf-8",
  use_iconv = TRUE
)  %>% fortify() 

province_city <- read.csv(
  "F:/R/Rstudy/chinaprovincecity.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
) 

###构造线条起始点数据：
city<-c("北京","上海","重庆","天津","武汉","南京","广州","沈阳","西安","郑州")

city_data <- merge(city,city) %>% rename(Start=x,End=y) %>% arrange(Start)#合并city，产生两列城市列，一列为Start,一列为End
city_data <- city_data %>% filter(Start != End)#筛选Start与End列不相等的行数据
#下面构造起点城市与终点城市的位置（经纬度）作为直线线的起点与终点坐标
city_data <- city_data %>% merge(province_city[,c("city","jd","wd")],by.x="Start",by.y="city",all.x=TRUE) %>% rename(Start_long=jd,Start_lat=wd)
city_data <- city_data %>% merge(province_city[,c("city","jd","wd")],by.x="End",by.y="city",all.x=TRUE) %>% rename(End_long=jd,End_lat=wd)
city_data <- transform(   #增加三个指标数据
  city_data,
  zhibiao1=runif(nrow(city_data),0,100),
  zhibiao2=runif(nrow(city_data),0,100),
  zhibiao3=runif(nrow(city_data),0,100)
)
#下面先考虑要绘制N个城市1个指标的情况
#思考：如何画所有城市的直线段图？直接增加一个geom_segment，将所有的起点与终点坐标进行一次性绘制，效果如何？
ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",colour="grey60") +
  geom_segment(data=city_data,aes(x=Start_long,y=Start_lat,xend=End_long,yend=End_lat,size=zhibiao1),colour="black")+
  coord_map("polyconic") + 
  scale_size_area(max_size=2) +
  theme_void()

###最合适的做法1：图形分面，并将指标映射到散点上（因此要增加散点图）

ggplot()+
  geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",colour="grey60")+
  geom_segment(data=city_data,aes(x=Start_long,y=Start_lat,xend=End_long,yend=End_lat),colour="black")+
  #为所有终点城市增加指标数据，即散点图，散点大小按zhibiao1大小设置，问：散点图的位置数据应该是起点城市还是终点城市的位置？
  geom_point(data =city_data,aes(x=End_long,y=End_lat,size=zhibiao1),shape=21,fill="#8E0F2E",colour="black",alpha=0.4)+
  scale_size_area(max_size=6)+
  coord_map("polyconic") + 
  facet_wrap(~Start,nrow = 2)+ #按城市分面,所有图片分成2行
  theme_void()

#思考：上面图形只是显示了各个城市zhibiao1的区别，如何能显示zhibiao2与zhibiao3

###最合适的做法2——Shiny动态交互图：控制显示3个zhibiao的情况，供参考

city_list <- list("北京"="北京","上海"="上海","重庆"="重庆","天津"="天津","武汉"="武汉","南京"="南京","广州"="广州","沈阳"="沈阳","西安"="西安","郑州"="郑州")

ui <-shinyUI(fluidPage(  #设置界面布局
  theme=shinytheme("cerulean"),
  titlePanel("Population Structure Data"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("var1","City",city_list,inline=FALSE),
      selectInput("var2","Value",c("zhibiao1"="zhibiao1","zhibiao2"="zhibiao2","zhibiao3"="zhibiao3"),selected="zhibiao1")
    ),
    mainPanel(h2("Trade Stream"),plotOutput("distPlot"))
  )
))

server <- shinyServer(function(input,output){ #服务器端脚本程序设置
  output$distPlot <- renderPlot({
    mydata=filter(city_data%>%filter(Start==input$var1))
    argu<-switch(input$var2,zhibiao1=mydata$zhibiao1,zhibiao2=mydata$zhibiao2,zhibiao3=mydata$zhibiao3)
    ggplot(mydata)+
      geom_polygon(data=china_map,aes(x=long,y=lat,group=group),fill="white",colour="grey60")+
      geom_segment(aes(x=Start_long,y=Start_lat,xend=End_long,yend=End_lat),colour="black")+
      geom_point(aes(x=End_long,y=End_lat,size=argu),shape=21,fill="#8E0F2E",colour="black",alpha=0.4)+
      scale_size_area(max_size=6)+
      coord_map("polyconic") + 
      theme_void()
  })
})
shinyApp(ui=ui,server=server) #调用运行脚本代码，显示效果图


####11.4 ggmap背景+ggplot2图层混合应用####

library("dplyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("maps")
library("Cairo")
library("showtext")
library("baidumap")
#baidumap加载使用
library(baidumap)
options(baidumap.key ='fengzhiMaMa')#其中，wyky365为个人在百度地图平台申请的账号名http://lbsyun.baidu.com（baidumap库下载地址https://github.com/badbye/baidumap）

library("grid")
rm(list = ls())
gc()

#devtools::install_github("dkahle/ggmap")
#devtools::install_github("hadley/ggplot2")
#如下代码若出错，请尝试请更新ggplot2、ggmap至最新开发板！
#问题详情请参见Stack Overflow 主页：
#https://stackoverflow.com/questions/40642850

bbox_everest <- c(left =60, bottom =10, right =150, top =60)
mapdata <- get_stamenmap(bbox_everest, zoom =5)
ggmap(mapdata)
#案例1：基于google地图的放射状路径图

city_list <- c("西安","西宁","郑州","重庆","成都","石家庄","兰州","济南","大同","咸阳","包头","长沙")
source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)

address$lonx <- address[address$address =="长沙","lng"]#
address$laty <- address[address$address =="长沙","lat"]
address <- address[address$address!= "长沙",]
names(address)[2:3]<-c("lon","lat")
address$Num<-round(runif(11,50,100),2)

ggmap(mapdata)+
  geom_segment(data=address,aes(x=lon,y=lat,xend=lonx,yend=laty),size=0.3,colour="#FF6833")+
  geom_point(data=address,aes(x=lon,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  geom_point(data=NULL,aes(x=112.97935,y=28.21347),shape=21,size=8,fill=NA,col="steelblue")+
  scale_size_area(max_size=8)+ 
  theme_nothing()

#案例2：基于google地图的路径流向图
city_list <- c("海口","广州","长沙","武汉","郑州","石家庄","北京","沈阳","长春","哈尔滨")
source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(10,50,100),2)

ggmap(mapdata)+
  geom_path(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+
  geom_point(data=address,aes(x=lng,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  guides(fill=FALSE)+
  scale_size_area(max_size=8)+ 
  theme_nothing()

#案例3：基于google地图的闭环路径图
bbox_everest <- c(left =60, bottom =10, right =150, top =60)#指定图片大小的中国地图
mapdata <- get_stamenmap(bbox_everest, zoom =5)#获取相关的图片素材，zoom表示级别，其值越大，代表获取的经纬度越细，图片越清晰
ggmap(mapdata)#预览图片效果

city_list <- c("兰州","成都","重庆","贵阳","昆明","南宁","海口","广州","福州","上海","青岛","石家庄","呼和浩特","银川")
city_list <- c(city_list,city_list[1])

source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")
address <- GetJD(city_list)
address$Num<-round(runif(nrow(address),50,100),2)

ggmap(mapdata)+#ggmap代替ggplot以及geom_polygon函数，其他图层一样
  geom_path(data=address,aes(x=lng,y=lat),size=0.3,colour="#FF6833")+
  geom_point(data=address,aes(x=lng,y=lat,size=Num),shape=21,fill="#ED7D31",col="#E02939",alpha=.6)+
  guides(fill=FALSE)+
  scale_size_area(max_size=8)+ 
  theme_nothing()

####11.5 地图+mini字体地图应用####

#导入数据：
#导入字母与各省行政区域对应关系的数据，并手工生成用于设置各省小版图颜色的变量
mymapdata <- read.csv("F:/R/第十一章/EyesAsia.csv",stringsAsFactors=FALSE,check.names=FALSE)
mymapdata<-transform(
  mymapdata,
  scale=5, #增加scale列维度，问，此变量有什么用？
  peform=runif(43,20,50) #增加peform列维度，用于确定字体（各省地貌小图）颜色深浅
  )

#由于需要对字体（各省地貌小图）设置差异明显的颜色，因此不用渐变（连续变量）而用类别（离散变量）
#下面对peform列数据进行等距分箱
mymapdata$group <- cut( 
  mymapdata$peform,
  breaks=c(20,26,32,38,44,50),
  labels=c("20~26","26~32","32~38","38~44","44~50"),
  order=TRUE
  )

#去除不属于中国行政区域的国家或地理位置的数据
word<-c("日本","蒙古","朝鲜","韩国","青海湖","鄱阳湖","洞庭湖","太湖","洪泽湖")
mymapdata <- mymapdata %>% filter(!Cname %in% word) 

#按perform值的大小对mymapdata进行降序排序，并增加一个order变量用于控制显示各区域的先后顺序
mymapdata<-arrange(mymapdata,-peform) 
mymapdata$order=1:nrow(mymapdata)

#下载（求字网http://www.qiuziti.com）安装EyesAsia后，加载字体到R环境
#unzip("F:/R/第十一章/EyesAsia-Regular.zip");
font.add("myfont", "EyesAsia-Regular.otf");
#font.add("myfont","EyesAsia-Regular.otf")#加载EyesAsia字体
font_add("myyh","msyhl.ttc")#加载微软雅黑字体

#生成字体（行政区域）标签图层（生成的效果是环形，需要用到极坐标）,在R语言中显示不出来，导出即可看见
p1<- ggplot(mymapdata,aes(order,scale,label = case))+#order用于控制字体（即各区域）的先后顺序，scale用于后面控制显示极坐标的半径，label用于设置字体显示的样式
  ylim(-6,6)+
  coord_polar(theta="x",start=0)+ #转换坐标系，设置映射参数中y(在此就是scale列)为半径，旋转角度从0开始
  geom_text(aes(colour = group),family="myfont",size=20)+#增加标签图层，每个行政区域版图的颜色
  scale_colour_brewer(palette="Greens",guide=FALSE)+#填充热力地图
  theme_void()


#下面构造中国热力填充图
china_map <- readOGR("F:/R/Rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)

mydata1   <- china_map@data %>% mutate(id = row.names(.)) %>% select(id,NAME)   
china_map1 <- fortify(china_map)  %>% select(id,long,lat,group,order)         
china_map_data <- left_join(china_map1,mydata1,by = "id")                     

mydata2   <- read.csv("F:/R/Rstudy/geshengzhibiao.csv",stringsAsFactors = FALSE)   
mydata2$zhibiao <- as.numeric(mydata2$zhibiao)
final_data <- left_join(china_map_data,mydata2,by = "NAME") %>% select(-province)         

#1、常见的热力填色地图：
p2<- ggplot() +
  geom_polygon(data = final_data,aes(x = long,y = lat, group = group,fill = zhibiao),colour="grey40") +
  scale_fill_distiller(palette = "BrBG",na.value = "white") +  
  guides(fill=FALSE)+
  coord_map("polyconic") + 
  theme_void()
  


CairoPNG("F:/R/chineserador1.png",900,900)#导出图片
showtext_begin()

vs <- viewport(width=0.95,height=0.95,x=0.5,y=0.5)#设置窗口大小用于显示字体标签层    
print(p1,vp = vs)  
vs <- viewport(width=0.75,height=0.8,x=0.5,y=0.5)#设置窗口大小用于显示中国热力填充图层   
print(p2,vp=vs)

showtext_end()
dev.off()


CairoPNG("F:/R/chineserador2.png",900,900)#只显示字体标签（各省区域版图环形图）
#思考：为什么这里不需要设置viewpoint（窗口）大小
showtext_begin()
ggplot(mymapdata,aes(order,scale,label = case))+
  ylim(-6,6)+
  coord_polar(theta="x",start=0)+
  geom_text(aes(colour = group),family="myfont",size=20)+
  scale_colour_brewer(palette="Greens",guide=FALSE)+
  theme_void()
showtext_end()
dev.off()


CairoPNG("F:/R/chineserador3.png",900,900)#只显示中国版图
showtext_begin()
ggplot() +
  geom_polygon(data = final_data,aes(x = long,y = lat, group = group,fill = zhibiao),colour="grey40") +
  scale_fill_distiller(palette = "BrBG",na.value = "white") +  
  guides(fill=FALSE)+
  coord_map("polyconic") + 
  theme_void()
showtext_end()
dev.off()
-
  


####11.6 基于ggplot2的等值线密度图####

##思考：等值密度图由哪些图层构成？
rm(list=ls())
gc()
library("dplyr")
library("dplyr")
library("ggplot2")
library("ggmap")
library("rgdal")
library("maps")
library("Cairo")
library("showtext")
library("baidumap")
library("grid")

china_map <- readOGR("F:/R/Rstudy/CHN_adm/bou2_4p.shp",stringsAsFactors = FALSE)

mydata1   <- china_map@data %>% mutate(id = row.names(.)) %>% select(id,NAME)   
china_map1 <- fortify(china_map)  %>% select(id,long,lat,group,order)         
china_map_data <- left_join(china_map1,mydata1,by = "id")                     

cityname <- c("北京", "天津", "哈尔滨", "乌鲁木齐", "西宁", "兰州", "银川", "呼和浩特", "石家庄", "太原", 
            "沈阳", "长春", "济南", "拉萨", "成都", "昆明", "西安", "郑州", "重庆", "武汉", "长沙", "贵阳", "南京", 
            "合肥", "上海", "杭州", "南昌", "福州", "广州", "南宁", "海口", "台北", "香港", "澳门", "厦门", "青岛", 
            "大连",  "无锡", "桂林")
source("F:/R/第十一章/web_city_adress.r",encoding = "utf-8")
address <- GetJD(cityname)

###纯ggplot2静态图


ggplot() +
  geom_polygon(data = china_map_data,aes(x = long,y = lat, group = group),colour="grey40",fill = "white") +
  geom_point(data = address,aes(x = lng,y = lat),colour = "red") +
  geom_polygon(data = address,aes(x=lng,y=lat,fill = ..level..), stat = "density_2d", alpha = .3, color = NA)+ 
  #fill = ..level..表示按照多边形填充颜色
  scale_fill_distiller(palette = "Reds",na.value = "white",direction = 1) +  
  guides(fill=FALSE)+
  coord_map("polyconic") + 
  theme_void()

#思考：以上图片可否增加地貌效果？如果要增加地貌效果，需要哪个函数？

###基于googlemap的密度热度图
bbox_everest <- c(left =60, bottom =10, right =150, top =60)
mapdata <- get_stamenmap(bbox_everest, zoom =5)


ggmap(mapdata) +
  geom_polygon(data = address,aes(x=lng,y=lat,fill = ..level..), stat="density_2d", alpha = .3, color = NA)+ 
  #fill = ..level..表示按照多边形填充颜色
  geom_point(data = address,aes(x = lng,y = lat),colour = "red") +
  scale_fill_distiller(palette = "Reds",na.value = "white",direction = 1) +  
  guides(fill=FALSE)+
  theme_nothing()


####END####

