setwd("d:/")
data<-read.csv("recruitment.csv",header=TRUE,stringsAsFactors = FALSE)
ad<-data[which(data$职位月薪=="广告文案策划"),]
#去掉面议数据
data1<-data[-which(data$职位月薪=="面议"),]
data<-data[!str_detect(data$职位月薪,"面议"),]

require(stringr)
one<-data[!str_detect(data$职位月薪,"-"),]
two<-data[str_detect(data$职位月薪,"-"),]
thr<-data[str_detect(data$职位月薪,"以上"),]
four<-data[str_detect(data$职位月薪,"以下"),]

salary.one<-as.integer(str_extract(one$职位月薪,"[0-9]{1,}"))

salary.two<-str_extract_all(two$职位月薪,"[0-9]{3,}")
s<-str_extract(two$职位月薪,"[0-9]{3}")
class(salary.two)

require(plyr)
del.code<-str_extract(two$职位月薪,"[0-9]{1,}-[0-9]{1,}元/月")
salary<-str_extract_all(del.code,"[0-9]{1,}")
salary<-ldply(salary)

#计算带有范围的职位月薪的均值
salary<-data.frame(apply(salary,2,as.integer))
class(salary)
salary$avg<-apply(salary,1,mean)
