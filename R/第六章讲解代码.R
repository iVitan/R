
###6.3  美学映射参数书写规则###
library("showtext")
library("ggplot2")
library("magrittr")
library("reshape2")
mydata1<-data.frame(x=1:10,y=runif(10,0,100))  #runif函数是用来做什么的？
mydata2<-data.frame(x=1:10,y=runif(10,0,100))
mydata3<-data.frame(x=1:10,y=runif(10,0,100))
mydata1$class<-sample(LETTERS[1:3],10,replace=T) #此行代码会产生什么结果？
mydata1$x1<-runif(10,0,100)
mydata1$y1<-runif(10,0,100)
mydata1

#全部共享：

ggplot(data = mydata1, mapping = aes(x,y))+
  geom_point(size=5,shape=21,colour="black")  #geom_point的data与mapping参数值分别是？

ggplot()+             #此段代码与上面的代码运行的结果是否相同？
  geom_point(        #此段代码与上面的代码不同的方面主要是？
    data = mydata1,aes(x,y), 
    size=5,
    shape=21,
    colour="black"
  )

ggplot(mydata1,aes(x,y))+
  geom_point(size=5,shape=21,colour="black")+
  geom_line()       #geom_line是用来做什么？

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")   +
  geom_line(data=mydata1,aes(x=x,y=y))      #geom_line里面的data与aes参数值是否可以去掉？


#只共享数据源：

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")+#geom_point与geom_line里面的参数是否一样？
  geom_line(data=mydata1,aes(x=x1,y=y1))


ggplot(mydata1)+
  geom_point(aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")+
  geom_line(aes(x=x1,y=y1))


#不共享任何成分：

ggplot()+
  geom_line(data=mydata1,aes(x=x,y=y),colour="black")+
  geom_line(data=mydata2,aes(x=x,y=y),colour="red")+
  geom_line(data=mydata3,aes(x=x,y=y),colour="blue")


#美学映射参数写在aes函数内部与写在外部的区别

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y),colour="black",size=5)#美学映射参数写在aes函数外部表示什么意思？

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y,colour=x1),size=5)#美学映射参数写在aes函数内部表示什么意思？

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y,colour=x1,size=x))


####6.4 位置调整参数应用规则####

data <- data.frame(
  Conpany = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2015 = c(5000,3500,2300,2100,3100),
  Sale2016 = c(5050,3800,2900,2500,3300)
)

library(reshape2)
library(ggplot2)

mydata <- melt(data,id.vars="Conpany",variable.name="Year",value.name="Sale")	#为什么对data进行转换？

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               #fill=Year的作用是什么：
  geom_bar(stat="identity",alpha=.5)                      #stat="identity"的作用是什么：

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               #position="identity"的作用是什么：
  geom_bar(stat="identity",position="identity",alpha=.5) 

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               #position=position_identity()与position="identity"有什么区别：
  geom_bar(stat="identity",position=position_identity(),alpha=.5) 

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # position='dodge'的作用是什么：
  geom_bar(stat="identity",position='dodge')

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # position_dodge后面的参数大小代表什么意思？
  geom_bar(stat="identity",position=position_dodge(0.5))


ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # width后面的参数大小代表什么意思 ：
  geom_bar(stat="identity",width = 0.8,position=position_dodge(0.9))



ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # position="stack"的作用是什么：	
  geom_bar(stat="identity",position="stack")

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # 多序列堆积柱形图：	
  geom_bar(stat="identity",position=position_stack())



ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # 多序列百分比堆积柱形图的作用是什么？	
  geom_bar(stat="identity",position="fill")
ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # 多序列百分比堆积柱形图：	
  geom_bar(stat="identity",position=position_fill())


ggplot(mydata,aes(Conpany,Sale,fill=Year))+                         # 什么时候需要用分面柱形图？	
  geom_bar(stat="identity")+
  facet_grid(.~Year)                         # facet_grid(.~Year)与facet_grid(Year~.) 的区别是？	


####6.5  图表数据标签位置调整规则####
library('ggthemes')

data <- data.frame(
  Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
  Conpany = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2013 = c(5000,3500,2300,2100,3100),
  Sale2014 = c(5050,3800,2900,2500,3300),
  Sale2015 = c(5050,3800,2900,2500,3300),
  Sale2016 = c(5050,3800,2900,2500,3300)
)

mydata <- melt(
  data,
  id.vars=c("Name","Conpany"),
  variable.name="Year",
  value.name="Sale"
)

#单序列数据标签：
ggplot(data,aes(Conpany,Sale2016,fill= Conpany))+      #此图画完后有什么不足的地方？
  geom_bar(stat="identity")+
  scale_fill_wsj()+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


ggplot(data,aes(Conpany,Sale2016,fill= Conpany,label =Sale2016))+  #此段代码与上面的代码多了哪些？
  geom_bar(stat="identity")+
  geom_text(vjust=-0.5) +                                        #geom_text图层在这里的作用是什么？为何要设置vjust=-0.5？
  scale_fill_wsj()+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )




####多序列数据标签：

ggplot(mydata,aes(Conpany,Sale,fill=Year))+         #fill=Year在此去掉后产生的图有什么区别？
  geom_bar(stat="identity",position="dodge")+
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), vjust = -0.5,position=position_dodge(0.9)) +
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+   #label =Sale在此去掉代码可否成功运行？它在此的作用是什么？
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = -0.5) +
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale + 0.05), position = 'stack', vjust = 0.5) +
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )



ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+  #此段代码运行的结果与上面的差别是什么？
  geom_bar(stat="identity",position="stack")+            #产生这些差别的关键参数是哪个？
  geom_text(aes(y = Sale + 0.05), position = 'stack', vjust = 0.5) +
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


####多序列数据标签——分面：

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), vjust = -0.5) +
  scale_fill_wsj("rgby", "")+
  facet_grid(.~Year)+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), vjust = -0.5) +
  scale_fill_wsj("rgby", "")+
  facet_grid(Year~.)+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


####6.6  图形颜色映射规则与因子变量的意义####
library('scales')
library('RColorBrewer')

ggplot(data,aes(Conpany,Sale2013,fill =Conpany ))+
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale2013 + 0.05,label = Sale2013), position = 'stack', vjust = 1) +
  scale_fill_manual(values = c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335'))+
  ggtitle("The Financial Performance of Five Giant")


'Tencent','Google','Facebook','Apple','Amozon'

show_col(c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335'),labels = FALSE)


ggplot(data,aes(Conpany,Sale2013,fill =Conpany))+
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale2013 + 0.05,label = Sale2013), position = 'stack', vjust = 1) +
  scale_x_discrete(                         #scale_x_discrete在这里的作用是？
    limits = c('Tencent','Google','Facebook','Apple','Amozon'),
    labels = c('Tencent','Google','Facebook','Apple','Amozon')
  ) +
  scale_fill_manual(                        #scale_fill_manual在这里的作用是？
    limits = c('Tencent','Google','Facebook','Apple','Amozon'),
    values = c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335')
  )+
  ggtitle("The Financial Performance of Five Giant")

data11 <- data
data11$Conpany1 <- factor(data11$Conpany,levels = c('Tencent','Google','Facebook','Apple','Amozon'),ordered = TRUE)

ggplot(data11,aes(Conpany1,Sale2013,fill =Conpany1 ))+  #本段代码与上面代码的区别是什么？
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale2013 + 0.05,label = Sale2013), position = 'stack', vjust = 1) +
  scale_fill_manual(values = c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335'))+
  ggtitle("The Financial Performance of Five Giant")


###


ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+  #为什么本段代码与上段代码相似，但产生的图形是堆积图？
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale + 0.05), position = 'stack', vjust = 1) +
  scale_fill_manual(values = c('#D3BA68','#D5695D','#5D8CA8','#65A479'))+
  ggtitle("The Financial Performance of Five Giant")

mydata$Year1 <- factor(
  mydata$Year,
  levels = c('Sale2016','Sale2015','Sale2014','Sale2013'),
  ordered = TRUE
)


show_col(c('#D3BA68','#D5695D','#5D8CA8','#65A479'),labels = FALSE)

ggplot(mydata,aes(Conpany,Sale,fill=Year1,label =Sale))+
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale + 0.05), position = 'stack', vjust = 0) +
  scale_fill_brewer(palette ='Blues' )+
  ggtitle("The Financial Performance of Five Giant")


#####6.6  笛卡尔坐标系下的分面应用#####

#横排分面（柱形）

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = -0.5) +
  scale_fill_wsj("rgby", "")+
  ggtitle("The Financial Performance of Five Giant")+
  facet_grid(.~Year) +
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


#纵向分面（条形）：         #画图时，为会么要分面？
                            #纵向与柱形分面的代码主要区别在哪里？

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = -0.5) +
  scale_fill_wsj("rgby", "")+
  ggtitle("The Financial Performance of Five Giant")+
  facet_grid(.~Year)+
  coord_flip() +
  theme_wsj()+
  theme(
    axis.title = element_blank(),
    legend.position='none',
    axis.ticks.length=unit(0.5,'cm')
  )



#柱形分面（竖排）：

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = 1) +
  facet_grid(Year~.) +
  scale_fill_wsj("rgby", "")+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.title = element_blank(),
    legend.position='none',
    axis.ticks.length=unit(0.5,'cm')
  )

#条形分面（竖排）：

ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = 0.5) +
  facet_grid(Year~.)+
  coord_flip() +
  scale_fill_wsj("rgby", "")+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.title = element_blank(),
    axis.ticks.length=unit(0.5,'cm'),
    legend.position='none'
  )

###facet_warp——缠绕分面：

ggplot(mydata,aes(Conpany,Sale,fill=Year))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~Year) + 
  scale_fill_wsj("rgby", "")+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.title = element_blank(),
    legend.position='none',
    axis.ticks.length=unit(0.5,'cm')
  )

####6.8  散点图（气泡图）、折线图（面积图）、箱线图、直方图案例####

#散点图（气泡图）

mydata1 <- diamonds[sample(nrow(diamonds),1000),]

#散点图（气泡图）的可用标度：
#大小——scale
#颜色——连续渐变/离散渐变
#形状——形状分类
#分面——支持使用分面（一般不适用于散点图的场合）


ggplot(mydata1,aes(carat,price,colour=cut,size=table))+
  geom_point()

ggplot(mydata1,aes(carat,price,colour=color,size=table))+
  geom_point()

ggplot(mydata1,aes(carat,price,colour=color,shape=color))+
  geom_point()

ggplot(mydata1,aes(carat,price,colour=color))+
  geom_point()+
  facet_grid(.~color)

ggplot(mydata1,aes(carat,price,colour=color))+
  geom_point()+
  facet_grid(color~.)

#折线图
library(ggplot2)
library(reshape2)
library(ggthemes)
library(RColorBrewer)


data<-data.frame(
  Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
  Company = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2013 = c(5000,3500,2300,2100,3100),
  Sale2014 = c(5050,3800,2900,2500,3300),
  Sale2015 = c(5050,4000,3200,2800,3700),
  Sale2016 = c(6000,4800,4500,3500,4300)
)

mydata<-melt(
  data,
  id.vars=c("Name","Company"),
  variable.name="Year",
  value.name="Sale"
)

###折线图 

ggplot(mydata,aes(Company,Sale,colour=Year))+      #如果X轴是一个类别型变量，需要指定group声明分类规则
  geom_line()

ggplot(mydata,aes(Company,Sale,group = Year ))+
  geom_line()

ggplot(mydata,aes(Company,Sale,group = Year,colour = Year))+
  geom_line()+
  geom_point()+
  geom_text(aes(label = Sale))

ggplot(mydata,aes(Company,Sale,group = Year))+   #数据标签颜色不变
  geom_line(aes(colour = Year))+
  geom_point(aes(colour = Year))+
  geom_text(aes(label = Sale))

ggplot(mydata,aes(Company,Sale,group = Year))+   #分面1
  geom_line(aes(colour = Year))+
  geom_point(aes(colour = Year))+
  facet_grid(.~Year)+
  geom_text(aes(label = Sale))


ggplot(mydata,aes(Company,Sale,group = Year))+   #分面2
  geom_line(aes(colour = Year))+
  geom_point(aes(colour = Year))+
  facet_grid(Year~.)+
  geom_text(aes(label = Sale))


#面积图：
library('tidyr')

mydata <- data.frame(
  date = 2014:2018,
  GMV_A = runif(5,1000,5000),
  GMV_B = runif(5,1000,5000),  
  GMV_C = runif(5,1000,5000),
  GMV_D = runif(5,1000,5000),
  GMV_E = runif(5,1000,5000)
) %>% gather(class,gmv,-1)

ggplot(mydata,aes(date,gmv,fill = class))+   #为什么数据标签位置不对？
  geom_area()+
  geom_text(aes(label = gmv))     #为什么数据标签位置不对？


ggplot(mydata,aes(date,gmv,group = class))+   #因为geom_text几何对象的默认位置调整参数是postion = 'identity',与geom_area默认位置参数不一致
  geom_area(aes(fill = class))+
  geom_text(aes(label = round(gmv,0)),position = "stack")+
  scale_fill_brewer(palette = 'Blues')


ggplot(mydata,aes(date,gmv,group = class))+   #可以使用colorbrewer配色库进行配优化
  geom_area(aes(fill = class))+
  geom_text(aes(label = round(gmv,0)),position = "stack")+
  scale_fill_brewer(palette = 'Blues')


ggplot(mydata,aes(date,gmv,group = class))+   #横向分面
  geom_area(aes(fill = class))+
  facet_grid(.~class)+
  geom_text(aes(label = round(gmv,0)))+
  scale_fill_brewer(palette = 'Blues')


ggplot(mydata,aes(date,gmv,group = class))+   #纵向分面
  geom_area(aes(fill = class))+
  facet_grid(class~.)+
  geom_text(aes(label = round(gmv,0)))+
  scale_fill_brewer(palette = 'Blues')


#直方图
data(diamonds)
set.seed(42)
small <- diamonds[sample(nrow(diamonds), 1000), ]


ggplot(small,aes(x=price))+
  geom_histogram(bins = 30)

ggplot(small,aes(x=price,fill=cut))+
  geom_histogram(bins = 30)

ggplot(small,aes(x=price,fill=cut))+
  geom_histogram(binwidth = 1000)

#以上两种方式等价

ggplot(small,aes(x=price,fill=cut))+
  geom_histogram(binwidth = 1000)+
  facet_grid(.~cut)

ggplot(small,aes(x=price,fill=cut))+
  geom_histogram(binwidth = 1000)+
  facet_grid(cut~.)


#箱线图：
ggplot(small,aes(1,price))+
  geom_boxplot()

ggplot(small,aes(cut,price))+
  geom_boxplot()


ggplot(small,aes(cut,price,fill=clarity))+
  geom_boxplot()


#横向分面
ggplot(small,aes(cut,price,fill=clarity))+
  geom_boxplot()+
  facet_grid(.~clarity)


#纵向分面
ggplot(small,aes(cut,price,fill=clarity))+
  geom_boxplot()+
  facet_grid(clarity~.)
