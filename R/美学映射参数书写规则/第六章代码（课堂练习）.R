###6.3  美学映射参数书写规则###
library("showtext")
library("ggplot2")
library("magrittr")
library("reshape2")
mydata1<-data.frame(x=1:10,y=runif(10,0,100))  #随机生成服从正态分布的10个数，最小值0，最大值100
mydata2<-data.frame(x=1:10,y=runif(10,0,100)) #随机生成服从正态分布的10个数，最小值0，最大值100
mydata3<-data.frame(x=1:10,y=runif(10,0,100)) #随机生成服从正态分布的10个数，最小值0，最大值100
mydata1$class<-sample(LETTERS[1:3],10,replace=T) #此行代码会产生什么结果？
mydata1$x1<-runif(10,0,100) #随机生成服从正态分布的10个数，最小值0，最大值100
mydata1$y1<-runif(10,0,100) #随机生成服从正态分布的10个数，最小值0，最大值100
mydata1

#全部共享：

ggplot(data = mydata1,mapping = aes(x,y))+#在ggplot主函数中设计data为mydata1与mapping的x为x,y为y,使得geom_point可以继承这些参数
  geom_point(size=5,shape=21,colour="black")  

ggplot()+             
  geom_point(        
    data = mydata1,mapping = aes(x,y), #在geom_point函数中设计data为mydata1与mapping的x为x,y为y,个性化设置geom_point参数
    size=5,
    shape=21,
    colour="black"
  )

ggplot(mydata1,aes(x,y))+
  geom_point(size=5,shape=21,colour="black")+
  geom_line()      #在以上图层添加x与y相同映射参数的折线

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")   +
  geom_line(data=mydata1,aes(x=x,y=y))##在geom_line函数中设计data为mydata1与mapping的x为x,y为y,个性化设置geom_line参数,geom_line里面的data与aes参数值是否可以去掉？


#只共享数据源：

#设置以下geom_point与geom_line函数，使得它们共享mydata1，但前者x为x,y为y,后者x为x1,y为y1
ggplot()+
  geom_point(data = mydata1,aes(x=x,y=1),size=5,shape=21,fill=NA,colour="black")+#geom_point与geom_line里面的参数是否一样？
  geom_line(data = mydata1,aes(x=x1,y=y1))

#设置以下ggplot,geom_point与geom_line三个函数，使得geom_point与geom_line共享mydata1，但前者x为x,y为y,后者x为x1,y为y1
ggplot(mydata1)+
  geom_point(aes(x=x,y=y),size=5,shape=21,fill=NA,colour="black")+
  geom_line(aes(x=x1,y=y1))


#不共享任何成分：

#设置以下三个geom_line函数，使得它们的data分别为mydata1、mydata2、mydata3，x均为x,y均为y
ggplot()+
  geom_line(data = mydata1,aes(x=x,y=y),colour="black")+
  geom_line(data = mydata1,aes(x=x,y=y),colour="red")+
  geom_line(data = mydata1,aes(x=x,y=y),colour="blue")


#美学映射参数写在aes函数内部与写在外部的区别

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y),colour="black",size=5)#要求所有的点均为"black"颜色

ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y,colour=x1),size=5)#要求所有的点的颜色根据x1列值大小来调整
ggplot()+
  geom_point(data=mydata1,aes(x=x,y=y,colour=x1,size=x1))#要求所有的点的颜色根据x1列值大小来调整，点的大小根据x列值大小来调整


####6.4 位置调整参数应用规则####

data <- data.frame(
  Conpany = c("Apple","Google","Facebook","Amozon","Tencent"),
  Sale2015 = c(5000,3500,2300,2100,3100),
  Sale2016 = c(5050,3800,2900,2500,3300)
)

library(reshape2)
library(ggplot2)

mydata <- melt(data,id.vars="Conpany",variable.name="Year",value.name="Sale")	#将data数据框转换成具有Conpany、Year、Sale三个维度的长表格

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               #设置柱形图每个企业按年份Year区分呈现：
  geom_bar(stat = "identity",alpha=.5)                      #柱形图统计变量默认为ggplot的aes函数的y

ggplot(mydata,aes(Conpany,Sale,fill=Year))+              
  geom_bar(stat="identity",position = "identity",alpha=.5)  #position不作任何调整

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               
  geom_bar(stat="identity",position = position_identity(),alpha=.5)#将position设置为position_identity()对比一下与上面有什么区别：

ggplot(mydata,aes(Conpany,Sale,fill=Year))+              
  geom_bar(stat="identity",position = "dodge") # 设置柱形为簇状形式

ggplot(mydata,aes(Conpany,Sale,fill=Year))+              
  geom_bar(stat="identity",position = position_dodge(0.8)) # 设置每一系列柱子以遮盖20%的方式显示柱形


ggplot(mydata,aes(Conpany,Sale,fill=Year))+               # width后面的参数大小代表什么意思 ：
  geom_bar(stat="identity",width = 0.7,position=position_dodge(0.9))# 设置每一系列柱子之间间隔30%



ggplot(mydata,aes(Conpany,Sale,fill=Year))+             	
  geom_bar(stat="identity",position = "stack")  # 设置条形图为堆积柱形图

ggplot(mydata,aes(Conpany,Sale,fill=Year))+               	
  geom_bar(stat="identity",position = position_stack())# 设置position为position_stack()并对比以上设置



ggplot(mydata,aes(Conpany,Sale,fill=Year))+              
  geom_bar(stat="identity",position = "fill") # 设置柱形图为百分比堆积条形图	
ggplot(mydata,aes(Conpany,Sale,fill=Year))+              
  geom_bar(stat="identity",position = position_fill()) # 设置position为position_fill()并对比以上设置	


ggplot(mydata,aes(Conpany,Sale,fill=Year))+ #设置相关参数，要求显示每个企业销售情况（按年呈现）
  geom_bar(stat="identity")+
  facet_grid(.~Year)                         # 按行对每年数据进行分面


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
  id.vars=c("Name","Conpany"), #将data转换成"Name","Conpany"，"Year"，"Sale"的长数据表
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


ggplot(data,aes(Conpany,Sale2016,fill= Conpany,label =Sale2016))+  #添加Sale2016数据标签，并适当调整标签在柱子上的高度
  geom_bar(stat="identity")+                                        
  geom_text(vjust=-0.5)+ #此处如何填写？                                     
  scale_fill_wsj()+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )

####多序列数据标签：

ggplot(mydata,aes(Conpany,Sale,fill=Year))+         #fill=Year在此去掉后产生的图有什么区别？没填充颜色
  geom_bar(stat="identity",position="dodge")+       #此图也存在什么不足：没有具体数据显示出来
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )

ggplot(mydata,aes(Conpany,Sale,fill=Year,label = Sale))+#在ggplot设置文字标签参数
  geom_bar(stat="identity",position = "dodge")+ #设置position，产生簇状条形图
  geom_text(aes(y = Sale + 0.05), vjust = -0.5,position = position_dodge(0.9)) +#设置position，产生与簇状条形图对应的文字标签
  scale_fill_wsj("rgby", "")+
  guides(fill=guide_legend(title=NULL))+
  ggtitle("The Financial Performance of Five Giant")+
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


# ggplot(mydata,aes(Conpany,Sale,fill=Year,???))+   #label =Sale在此去掉代码可否成功运行？它在此的作用是什么？
#   geom_bar(stat="identity",position="dodge")+
#   geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = -0.5) +
#   scale_fill_wsj("rgby", "")+
#   guides(fill=guide_legend(title=NULL))+
#   ggtitle("The Financial Performance of Five Giant")+
#   theme_wsj()+
#   theme(
#     axis.ticks.length=unit(0.5,'cm'),
#     axis.title = element_blank(),
#     legend.position='none'
#   )

ggplot(mydata,aes(Conpany,Sale,fill=Year,label = Sale))+#在ggplot设置文字标签参数
  geom_bar(stat="identity",position = "Stack")+ #设置position,产生堆积柱形图
  geom_text(aes(y = Sale + 0.05), position = "Stack", vjust = 0.5) +#设置position,产生堆积柱形图对应的文字标签
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
  facet_grid(.~Year)+                          #将簇状柱形图按行分面
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
  facet_grid(Year~.)+                       #将簇状柱形图按列分面
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

ggplot(data,aes(Conpany,Sale2013,fill =Conpany ))+#问：该图片x轴上的标签是按照什么顺序排序的？
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale2013 + 0.05,label = Sale2013), position = 'stack', vjust = -0.5) +
  scale_fill_manual(values = c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335'))+
  ggtitle("The Financial Performance of Five Giant")


'Tencent','Google','Facebook','Apple','Amozon'  #如果想将x轴上的标签按左手边的标签顺序排序，如何操作？

show_col(c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335'),labels = FALSE)

###以下有二种方法将x轴标签按指定顺序排序


##法一：
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


##法二：fill参数值指定为某一特定向量，即有序因子向量
data11 <- data #将data赋给data11
data11$Conpany1 <- factor(data11$Conpany,levels = c('Tencent','Google','Facebook','Apple','Amozon'),ordered = TRUE)#???处应如何填写？

ggplot(data11,aes(Conpany1,Sale2013,fill =Conpany1 ))+  #利用data11绘制堆积条形图，x轴为企业，并按不同企业进行颜色填充
  geom_bar(stat="identity",position="stack")+
  geom_text(aes(y = Sale2013 + 0.05,label = Sale2013), position = 'stack', vjust = 1) +
  scale_fill_manual(values = c('#D3BA68','#D5695D','#5D8CA8','#65A479','#EA4335'))+
  ggtitle("The Financial Performance of Five Giant")


###多序列有序因子堆积条形图：按指定年份排列顺序画图


ggplot(mydata,aes(Conpany,Sale,fill=Year,label = Sale))+  #将每个企业不同年份的销量画堆积条形图，要求每个企业每年销量条形颜色不同，且添加年份文字标签
  geom_bar(stat="identity",position="stack")+  #
  geom_text(aes(y = Sale + 0.05),  position = "stack", vjust = 1) + #调整position参数，产生正确的文字标签
  scale_fill_manual(values = c('#D3BA68','#D5695D','#5D8CA8','#65A479'))+
  ggtitle("The Financial Performance of Five Giant")

mydata$Year1 <- factor(   #在mydata里面产生有序因子Year1列，其顺序为'Sale2016','Sale2015','Sale2014','Sale2013'
  mydata$Year,
  levels = c('Sale2016','Sale2015','Sale2014','Sale2013'),
  ordered = TRUE
)


show_col(c('#D3BA68','#D5695D','#5D8CA8','#65A479'),labels = FALSE)

ggplot(mydata,aes(Conpany,Sale,fill = Year,label =Sale))+##利用以上产生的有序因子Year1对图片x轴类别顺序进行设置
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
  facet_grid(Year~.) +
  theme_wsj()+
  theme(
    axis.ticks.length=unit(0.5,'cm'),
    axis.title = element_blank(),
    legend.position='none'
  )


#纵向分面（条形）：         #画图时，为什么要分面？


ggplot(mydata,aes(Conpany,Sale,fill=Year,label =Sale))+
  geom_bar(stat="identity",position="dodge")+
  geom_text(aes(y = Sale + 0.05), position = position_dodge(0.9), vjust = -0.5) +
  scale_fill_wsj("rgby", "")+
  ggtitle("The Financial Performance of Five Giant")+
  facet_grid(.~Year)+
  facet_grid(.~Year)+ #将每个分布图打横翻转
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
mydata1<- diamonds[sample(nrow(diamonds),1000),]

#散点图（气泡图）的可用标度：
#大小——scale
#颜色——连续渐变/离散渐变
#形状——形状分类
#分面——支持使用分面（一般不适用于散点图的场合）


###散点图数据出错，无法正常显示
# ggplot(mydata1,aes(carat,price,colour=cut,size=table))+
#   geom_point()+
#   
#   ggplot(mydata1,aes(carat,price,colour=color,size=table))+
#   geom_point()
# 
# ggplot(mydata1,aes(carat,price,colour=color,shape=color))+
#   geom_point()
# 
# ggplot(mydata1,aes(carat,price,colour=color))+
#   geom_point()+
#   facet_grid(.~color)
# 
# ggplot(mydata1,aes(carat,price,colour=color))+
#   geom_point()+
#   facet_grid(color~.)

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

mydata<-melt(                  #将以上data转换成Name,Company,Year,Sale四个维度的数据框
  data,
  id.vars=c("Name","Company"),
  variable.name="Year",
  value.name="Sale"
)

###折线图 

ggplot(mydata,aes(Company,Sale,colour=Year))+      #如果X轴是一个类别型变量，需要指定group声明分类规则
  geom_line()

ggplot(mydata,aes(Company,Sale,group = Year,colour=Year ))+#设置group值，使得折线图按每个企业的销量进行分组显示
  geom_line()

ggplot(mydata,aes(Company,Sale,group = Year,colour=Year))+#按年份区别显示每个企业所有点、折线与文字标签
  geom_line()+
  geom_point()+
  geom_text(aes(label = Sale))#

ggplot(mydata,aes(Company,Sale,group = Year))+   #如果所有企业每年文字标签的颜色要求一样，怎么操作？
  geom_line(aes(color = Year))+
  geom_point(aes(color = Year))+
  geom_text(aes(label=Sale))


#以上图形在呈现每年各企业的销量对比图时，有什么不足之处？解决的办法可以是什么？
ggplot(mydata,aes(Company,Sale,group = Year))+   #分面1：按行分面
  geom_line(aes(colour = Year))+
  geom_point(aes(colour = Year))+
  facet_grid(.~Year)+
  geom_text(aes(label = Sale))


ggplot(mydata,aes(Company,Sale,group = Year))+   #分面2：按列分面
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
) %>% gather(class,gmv,-1)  #%>%在这里的作用是？参见https://zhidao.baidu.com/question/176755516361815724.html

ggplot(mydata,aes(date,gmv,fill = class))+   #面积图的作用是什么？
  geom_area()+                          # 为什么数据标签位置不对？
  geom_text(aes(label = gmv))     #为什么数据标签位置不对？


ggplot(mydata,aes(date,gmv,group = class))+ 
  geom_area(aes(fill = class))+ 
  geom_text(aes(label = round(gmv,0)),position = "stack")+ #调整参数postion,使得geom_area与geom_text默认位置参数一致
  scale_fill_brewer(palette = 'Blues')

?geom_area()
?geom
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

# ggplot(small,aes(x=price,fill=cut))+
#   geom_histogram(bins = 30)
# 
# ggplot(small,aes(x=price,fill=cut))+
#   geom_histogram(binwidth = 1000)

#以上两种方式等价

# ggplot(small,aes(x=price,fill=cut))+
#   geom_histogram(binwidth = 1000)+
#   facet_grid(.~cut)
# 
# ggplot(small,aes(x=price,fill=cut))+
#   geom_histogram(binwidth = 1000)+
#   facet_grid(cut~.)


#箱线图：
ggplot(small,aes(1,price))+
  geom_boxplot()

ggplot(small,aes(cut,price))+
  geom_boxplot()


# ggplot(small,aes(cut,price,fill=clarity))+
#   geom_boxplot()


#横向分面
# ggplot(small,aes(cut,price,fill=clarity))+
#   geom_boxplot()+
#   facet_grid(.~clarity)


#纵向分面
# ggplot(small,aes(cut,price,fill=clarity))+
#   geom_boxplot()+
#   facet_grid(clarity~.)
