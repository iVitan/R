setwd("d:/")
recruitment<-read.csv("data.csv",header=TRUE,stringsAsFactors = FALSE)

str(recruitment)
Data<-recruitment[,c("Salary","Education","WorkYear","City","CompanySize")]
#构建成事物
require(arules)
Data<-data.frame(apply(Data,2,as.factor))
trans<-as(Data,"transactions")
inspect(trans[1:5])

#构建模型
rules<-apriori(trans,parameter = list(support=0.001,confidence=0.5))
inspect(rules[1:5])

anyNA(Data)

rules.sort<-sort(rules,by="support")
inspect(rules.sort[1:5])
rules.sort.con<-sort(rules,by="confidence")
inspect(rules.sort.con[1:5])

#工作经验不限的规则
inspect(subset(rules,subset=lhs%in%"WorkYears=不限")[1：10])
workyear.rules<-subset(rules,subset=lhs%in%"WorkYear=不限")
wk.rules.sort(workyear.rules,by="confidence")
inspect(wk.rules.sort[1:20])

#8k-15k并工作经验不限的规则
salary.rules<-subset(rules,subset=lhs%in%'Salary=8k-15k')
salary.rules.sort<-sort(salary.rules,by="confidence")
inspect(salary.rules.sort[1:10])


#8k-15k并工作经验不限的规则
salary.rules<-subset(rules,subset=lhs%in%c("Salary=8k-15k","WorkYear=不限"))
salary.rules.sort<-sort(salary.rules,by="confidence")
inspect(salary.rules.sort[1:10])

#一线？二线？
first.tier.city<-c('北京',"上海","广州","深圳")
Nfirst.tier.city<-c("成都","杭州","武汉","重庆","南京","天津","苏州","西安","长沙","沈阳","青岛","郑州","大连","东莞","宁波")
second.tier.city<-c("厦门","福州","无锡","合肥","昆明","哈尔滨","济南","佛山","长春","温州","石家庄","南宁","常州","泉州","南昌","贵阳","太原","金华","珠海","惠州","徐州","烟台","嘉兴","南通","乌鲁木齐","绍兴","中山","台州","兰州","海口")
city<-c(first.tier.city,Nfirst.tier.city,second.tier.city)
fst.num<-rep("一线城市",length(first.tier.city))
Nst.num<-rep("新一线城市",length(Nfirst.tier.city))
sec.num<-rep("二线城市",length(second.tier.city))
class<-c(fst.num,Nst.num,sec.num)

require(stringr)
fst<-c('北京',"上海","广州","深圳")
Nfst<-c("成都","杭州","武汉","重庆","南京","天津","苏州","西安","长沙","沈阳","青岛","郑州","大连","东莞","宁波")
cities<-c(fst,Nfst)
data<-data.frame(city,class)
names(data)<-c("City",'class')
new.data<-merge(data,Data,by="City")
