setwd("D:\\课程\\案例\\网页推荐案例data")
######读入ask类型的数据,并删选用户与项目属性
ask_data =read.csv(file="D:\\课程\\数据分析师培训资料\\案例\\网页推荐案例data\\ask02.csv",header=T,stringsAsFactors =F)  
askitem= ask_data[,c(1,2)]

####将ask数据去重处理
##tail表示取后面观测，n=1表示取一个值
library(plyr)
item_ask=ddply(askitem, .(realIP,FULLURL), tail, n = 1)


##Recommender
require(recommenderlab)

####将数据转换为0-1二元型数据，即模型的输入数据集
ask=as(item_ask,"binaryRatingMatrix") 

##采用基于物品的协同过滤算法进行模型数据进行建模，形成模型
ask_re=Recommender(ask,method="IBCF")

####利用模型对原始数据集进行预测并获得推荐长度为5的结果
ask_p=predict(ask_re,ask,n=5) 

####将结果保存至工作目录下的文件中，需要将结果转换为list型。
#####对list型结果采用sink与print命令将其保存

sink("preask.txt")
print(as(ask_p,"list"))
sink()

# Random算法每次都随机挑选用户没有产生过行为的物品推荐给当前用户
# Popular算法则按照物品的流行度给用户推荐他没有产生过行为的物品中最热门个物品。
# IBCF算法是基于物品的协同过滤算法
########模型评价，离线测试
####将三种算法形成一个算法的list
algorithms <- list( "random items" = list(name="RANDOM", param=NULL),
                    "popular items" = list(name="POPULAR", param=NULL),
                    "item-based CF" = list(name="IBCF", param=NULL))

#####将数据以交叉检验划分成K=10份，9份训练，一份测试
######given表示用来进行模型评测的项目数量，（实际数据中只能取1）
ask_es <- evaluationScheme(ask, method="cross-validation", k=10, given=1)

#####下面是随机划分的方式，90%的作为训练集
#####split.es <- evaluationScheme(d, method="split",train=0.9, given=1)

###采用算法列表对数据进行模型预测与评价，其推荐值N取3, 5, 10, 15, 20,30
ask_results <- evaluate(ask_es, algorithms, n=c(3, 5, 10, 15, 20,30))

####画出评价结果的图形
plot(ask_results, "prec/rec",legend="topleft",cex=0.67)

###构建F1的评价指标
fvalue<- function(p,r){
  return(2*p*r/(p+r))  
}

####求两个模型的各个评价指标的均值，并将其转换为数据框的形式
ask_ind=ldply(avg(ask_results))

####将指标第一列有关于模型的名字重新命名
ask_ind[,1]=paste(ask_ind[,1],c( 3, 5, 10, 15, 20,30))

####选取计算F1的两个指标以及有关于模型的名字
temp_ask=ask_ind[,c(1,6,7)]

###计算两个模型的F1的指标，并将所有指标综合
ask_Fvalue=cbind(ask_ind,fvalue=fvalue(temp_ask[,2],temp_ask[,3]))

####将评价指标写入文件中
write.csv(ask_Fvalue,file="./askpredict_ind.csv")
