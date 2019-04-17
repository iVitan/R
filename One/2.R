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


#gongling
table(bankloan$seniority)
boxplot(bankloan$seniority)
seniority.group<-cut(bankloan$seniority,breaks = c(0,1,3,5,10,15,20,25,30) ,include.lowest = T)
bankloan<-cbind(bankloan,seniority.group)

#

