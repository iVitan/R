setwd("d://")
data<-read.csv("model.csv",header = TRUE,stringsAsFactors = FALSE)
df
data$是否窃漏电<-as.factor(data$是否窃漏电)
ind<-sample(1:2,size=nrow(data), replace = TRUE,prob=c(0.7,0.3))
traindata<-data[ind==1,]
testdata<-data[ind==2,]


library(tree)
tree.model<-tree(是否窃漏电~+电量趋势增长指标+线损指标+告警类指标,data=traindata)
plot(tree.model,type="uniform")
text(tree.model)

train.predict=predict(tree.model,type = "class")
test.predict=predict(tree.model,newdata=testdata,type="class")

train.predictdata=cbind(traindata,predictedclass=train.predict）
train.confusion=table(actual=traindata$是否窃漏电,predictedclass=train.predict)                       
test.predictdata=cbind(testdata,predictedclass=test.predict)
test.confusiom=table(actual=testdata$是否窃漏电,predictedclass=test.predict)

table(actual=testdata$是否窃漏电,predict=test.predict)
