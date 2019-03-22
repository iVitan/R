## !/user/bin/env RStudio 1.1.423
## -*- coding: utf-8 -*-
## assistant geomtry and Infographic

## 《R语言商务图表与数据可视化》


rm(list = ls())
gc()

########第八章：ggplot辅助几何对象与信息图基础########


####8.1 辅助图形操作辅助图形1——geom_rect矩形图####

library("showtext")
library("ggplot2")
library("magrittr")
library("reshape2")
library("ggthemes")
library('dplyr')

mydata <- data.frame(
  Lebal  = c("Point1","Point2","Point3","Point4","Point5"),
  xstart = c(5.5,15.7,19.5,37.2,36.9),
  xend   = c(9.7,28.1,24.6,44.6,47.1), 
  ystart = c(9.6,23.1,2.3,33.2,9.2),
  yend   = c(16.1,36.2,11.7,38.5,15.3),
  size   = c(12,48,30,11.5,28),
  class  = c("A","A","A","C","C")
)

ggplot(mydata)+
  geom_rect (aes(xmin = xstart,xmax=xend,ymin=ystart,ymax=yend , fill = class)) + #绘制自由矩形图，xmin与xmax分别为xstart与xend,y轴设置类似
  scale_fill_wsj()


#按照x轴进行圆周化：
ggplot(mydata)+
  geom_rect(aes(xmin = xstart,xmax = xend , ymin = ystart , ymax = yend , fill = class)) +
  scale_fill_wsj() +
  ylim(-10,40) +
  scale_x_continuous(expand = c(0,0)) +
  coord_polar(theta = 'x')


#按照y轴进行圆周化
ggplot(mydata)+
  geom_rect(aes(xmin = xstart,xmax = xend , ymin = ystart , ymax = yend , fill = class)) +
  scale_fill_wsj() +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar(theta = 'y')


#分面操作：
ggplot(mydata)+
  geom_rect(aes(xmin = xstart,xmax = xend , ymin = ystart , ymax = yend , fill = class)) +
  scale_fill_wsj() +
  facet_grid(.~class) + #按class值按行分面
  scale_y_continuous(expand = c(0,0)) 

  
####8.2 辅助图形操作辅助图形2——geom_segment直线段图####


mydata <- data.frame(
  Lebal  = c("Segment1","Segment2","Segment3","Segment4","Segment5"),
  xstart = c(3.5,4.4,8.3,13.2,20),
  ystart = c(5,2.7,4.6,2.2,4.7),
  xend   = c(7.5,8.7,21,25,23), 
  yend   = c(7.9,4.2,7.2,3.8,4.4),
  class  = c("A","A","A","C","C")
)

ggplot(mydata) +
  geom_segment(
    aes(
      x=xstart,
      y=ystart,
      xend=xend,
      yend=yend ,
      colour = class
      ),  #绘制直线段图，xmin与xmax分别为xstart与xend,y轴设置类似
    arrow = arrow(length = unit(0.5,"cm")),#添加箭头指示
    size = 1.5
  ) +
  scale_colour_wsj() 
  

#按照X轴圆周化
ggplot(mydata) +
  geom_segment(
    aes(
      x = xstart , 
      y = ystart , 
      xend = xend ,
      yend = yend  , 
      colour = class
    ),
    arrow = arrow(length = unit(0.5,"cm")),
    size = 1.5
  ) +
  scale_colour_wsj() +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar(theta = 'y')

#按照y轴圆周化
ggplot(mydata) +
  geom_segment(
    aes(
      x = xstart , 
      y = ystart , 
      xend = xend ,
      yend = yend  , 
      colour = class
    ),
    arrow = arrow(length = unit(0.5,"cm")),
    size = 1.5
  ) +
  scale_colour_wsj() +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar(theta = 'x')


#分面操作：
ggplot(mydata) +
  geom_segment(
    aes(
      x = xstart , 
      y = ystart , 
      xend = xend ,
      yend = yend  , 
      colour = class
    ),
    arrow = arrow(length = unit(0.5,"cm")),
    size = 1.5
  ) +
  facet_grid(.~class) + #按class值按行分面
  scale_colour_wsj() +
  scale_y_continuous(expand = c(0,0))


####8.3 辅助图形操作辅助图形3——geom_linerange线范围图####

mydata <- data.frame(
  Lebal  = c("linerange1","linerange2","linerange3","linerange4","linerange5"),
  xstart = c(3.5,7,12,16,20),
  ymin   = c(2.5,6.5,3,4.5,3.8),
  ymax   = c(7.5,9.5,9,13.5,4.2),
  class  = c("A","A","A","C","C")
)


ggplot(mydata) +
  GeomLinerange (aes(x=xstart,ymin=ystart,ymax=yend , colour = class) , size = 1.5) +#绘制线范围图x值设置为xstart,y的范围为[ymin,ymax]
  scale_colour_wsj() 
 
# 横纵轴互换：
ggplot(mydata) +
  geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5) +
  coord_flip()  + #横纵轴互换
  scale_colour_wsj() 


#按x轴圆周化：
ggplot(mydata) +
  geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5) +
  scale_colour_wsj() +
  scale_x_continuous(limits = c(0,25),expand = c(0,0)) +
  coord_polar(theta = 'x')

#按y轴圆周化：
ggplot(mydata) +
  geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5) +
  scale_colour_wsj() +
  scale_y_continuous(expand = c(0,0)) +
  coord_polar(theta = 'y')


#分面：
ggplot(mydata) +
  geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5) +
  scale_colour_wsj() +
  facet_grid(.~class)  +#按class按行分面
  scale_x_continuous(limits = c(0,25),expand = c(0,0)) 

ggplot(mydata) +
  geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5) +
  coord_flip()  +#横纵轴互换
  scale_colour_wsj() +
  facet_grid(.~class) +#按class按行分面
  scale_x_continuous(limits = c(0,25),expand = c(0,0)) 

####8.4 辅助图形操作辅助图形4——geom_ploygon多边形图####

mydata <- data.frame(
  long = c(15.4,17.2,19.7,15.9,7.4,8.9,8.5,10.4,11.3,9.7,4.8,3.7,22.4,25.6,27.8,25.1,16.7,15.9,29.9,38.7,43.2,40.2,35.6,29.4),
  lat  = c(38.1,36.2,33.1,24.6,29.0,33.6,12.1,11.7,8.9,6.1,5.7,9.1,8.4,7.6,5.7,3.9,4.3,5.9,32.6,31.8,27.6,22.3,24.5,29.6),
  group= c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4),
  order =c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6),
  class = rep(c("A","c"),each = 12)
)

#
ggplot(mydata) +
  geom_polygon(aes(x = long , y = lat , group  = group , fill = class),colour = "white") +#绘制多边形图，x为long , y 为lat , group为group , fill为class
  scale_fill_wsj()

#按照X轴圆周化：
ggplot(mydata) +
  geom_polygon(aes(x = long , y = lat , group  = group , fill = class),colour = "white") +
  coord_polar(theta = 'x') +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_wsj()

#按照y轴圆周化：
ggplot(mydata) +
  geom_polygon(aes(x = long , y = lat , group  = group , fill = class),colour = "white") +
  coord_polar(theta = 'y') +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_wsj()

#分面：

ggplot(mydata) +
  geom_polygon(aes(x = long , y = lat , group  = group , fill = class),colour = "white") +
  facet_grid(~.class)+#按class按行分面
  scale_fill_wsj()

