setwd("d:/")
Data<-read.csv("model.csv",header = TRUE,stringsAsFactors = FALSE)
Data$是否窃漏电<-as.factor(Data$是否窃漏电)
Data<-Data[,3:6]


set.seed(1234)
ind<-sample(1:2,size = nrow(Data),replace = TRUE,prob = c(0.8,0.2))
trainData<-Data[ind==1,]
testData<-Data[ind==2,]

str(Data)

require(tree)
tree.model<-tree(是否窃漏电~.,data=trainData)
plot(tree.model)
text(tree.model)

test.predict<-predict(tree.model,newdata = testData)

compare<-data.frame(actual=testData$是否窃漏电,predict=predict(tree.model,newdata = testData,type="class"))
head(compare)

tree.confusion<-table(actual=testData$是否窃漏电,predict=compare$predict)
tree.confusion

tree.accu<-sum(diag(tree.confusion))/sum(tree.confusion)
tree.accu


require(nnet)

nnet.model<-nnet(是否窃漏电~.,trainData,size=4,decay=0.05)

summary(nnet.model)

nnet.predict<-predict(nnet.model,newdata = testData,type = 'class')
nnet.compare<-data.frame(actual=testData$是否窃漏电,predict=predict(nnet.model,newdata = testData,type="class"))
head(nnet.compare)

nnet.confusion<-table(actual=testData$是否窃漏电,predict=nnet.compare$predict)

nnet.accu<-sum(diag(nnet.confusion))/sum(nnet.confusion)
nnet.accu
tree.accu


require(ROCR)
par(mfrow=c(1,2))

tree.predition<-prediction(test.predict[,2],testData$是否窃漏电)
tree.perfor<-performance(tree.predition,"tpr","fpr")
plot(tree.perfor,type="l",lty=2,col="red")

tree.perfor.auc<-performance(tree.predition,measure = "auc")
tree.perfor.auc@y.values[[1]]
abline(0,1)

par(new=T) 
nnet.predition<-prediction(predict(nnet.model,testData),testData$是否窃漏电)
nnet.perfor<-performance(nnet.predition,"tpr","fpr")
plot(nnet.perfor,type="l",lty=2,col="blue")

nnet.perfor.auc<-performance(nnet.predition,measure = 'auc')
nnet.perfor.auc@y.values


tree.perfor.PR<-performance(tree.predition,"prec","rec")
plot(tree.perfor.PR,type="l",lty=2,col="red")


#神经网络模型
par(new=T)
nnet.perfor.PR<-performance(nnet.predition,"prec","rec")
plot(nnet.perfor.PR,type="l",lty=2,col="blue")
abline(1,-1)

