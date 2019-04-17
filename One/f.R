getwd()
setwd('e:/')
bankloan<-read.csv("bankloan.csv",header=TRUE)
str(bankloan)
bankloan$education<-as.factor(bankloan$education)
str(babkloan)

require(ggplot2)
ggplot(data=bankloan,aes(age,seniority))+geom_boxplot()

table(bankloan$age)
age.group<-c()
for (i in 1:length(bankloan$age)){
  if(bankloan$age[i]<=30)  {age.group[i]="达年"}
  else if(bankloan$age[i]>30 & bankloan$age[i]<=40)  {age.group[i]="盛年"}
  else if(bankloan$age[i]>40 & bankloan$age[i]<=50)  {age.group[i]="旺年"}
  else {age.group[i]="老年"}
}
bankloan<-cbind(bankloan,age.group)



#way2
age.group2<-cut(bankloan$age,breaks = paste0(2:6,0),include.lowest = T)
bankloan<-cbind(bankloan,age.group2)
boxplot(bankloan$age)


#工龄
table(bankloan$seniority)
boxplot(bankloan$seniority)
seniority.group<-cut(bankloan$seniority,breaks = c(0,1,3,5,10,15,20,25,30) ,include.lowest = T)
bankloan<-cbind(bankloan,seniority.group)

#2017.4.19
setwd("e://")
bankloan<-read.csv("bankloan.csv",header = TRUE)
bankloan$age.group<-cut(bankloan$age,breaks = seq(20,60,10),include.lowest = T)
bankloan$seniority.group<-cut(bankloan$seniority,breaks = c(0,1,3,5,7,10,15,20,30,35),include.lowest = T)


bankloan$debt<-bankloan$income*bankloan$debt_rate/100
bankloan$education.group<-cut(bankloan$education,breaks = c(1,2,3,4,5),include.lowest = T)
library(ggplot2)
ggplot(data = bankloan,aes(income,fill=age.group))+geom_histogram(bins=5)+facet_grid(~age.group)
ggplot(data = bankloan,aes(debt,fill=age.group))+geom_histogram(bins = 30)+facet_grid(~age.group)
ggplot(data = bankloan,aes(income,fill=education.group))+geom_histogram(bins = 30)+facet_grid(~education.group)
ggplot(data = bankloan,aes(debt,fill=education.group))+geom_histogram(bins =30 )+facet_grid(~education.group)
ggplot(data = bankloan,aes(income,fill=seniority.group))+geom_histogram(bins = 30)+facet_grid(~seniority.group)
ggplot(data = bankloan,aes(debt,fill=seniority.group))+geom_histogram(bins = 30)+facet_grid(~seniority.group)
ggplot(data = bankloan,aes(income,fill=age.group))+geom_density()+facet_grid(~age.group)
ggplot(data = bankloan,aes(debt,fill=age.group))+geom_density()+facet_grid(~age.group)
ggplot(data = bankloan,aes(income,fill=education.group))+geom_density()+facet_grid(~education.group)
ggplot(data = bankloan,aes(debt,fill=education.group))+geom_density()+facet_grid(~education.group)
ggplot(data = bankloan,aes(income,fill=seniority.group))+geom_density()+facet_grid(~seniority.group)
ggplot(data = bankloan,aes(debt,fill=seniority.group))+geom_density()+facet_grid(~seniority.group)


ggplot(data=bankloan,aes(income,debt))+geom_point()+geom_smooth(method=lm)+facet_grid(~age.group)

