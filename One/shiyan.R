setwd("d://")
data<-read.csv("model.csv",header = TRUE,stringsAsFactors = FALSE)
data$是否窃漏电<-as.factor(data$是否窃漏电)
ind<-sample(1:2,size=nrow(data), replace = TRUE,prob=c(0.7,0.3))
traindata<-data[ind==1,]
testdata<-data[ind==2,]

library(tree)
tree.model<-tree(是否窃漏电~.,data=traindata)
plot(tree.model,type="uniform")
text(tree.model)
