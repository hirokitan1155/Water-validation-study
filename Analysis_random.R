### read data ###

setwd("/Users/hiroki-tan/NAIST AHC Dropbox/tanaka hiroki/work/Water/validation/Kobe_trial_CRO/再学習用データ_20220615/data/")

getwd()


library("Metrics")
library(rpart)
library(rpart.plot)
library("UBL")
library(randomForest)

######
T<-read.table("male_14_total.csv",sep=",",header=TRUE)
analysis="Total"
gender="Male" #Female of Male
Num=14
######

T<-T[,c(1:Num,15)] #10 or 15
Lab=dim(T)[2]
#####################

#nr<-nrow(T)
#row.train <- sample(nr, floor(0.6 * nr))

#write.table(row.train, "./dataset/train_female.txt", quote=FALSE, col.names=FALSE,row.names=FALSE,append=FALSE)

if (gender=="Female"){
	row.train <- read.table("../dataset/train_female.txt")[,1]
}else{
	row.train <- read.table("../dataset/train_male.txt")[,1]
}



Train<-rbind(T[row.train,])
Sub<-rbind(T[-row.train,])

# second split
#nr2<-nrow(Sub)
#row.train2 <- sample(nr2, floor(0.4 * nr2))

#write.table(row.train2, "./dataset/test_female.txt", quote=FALSE, col.names=FALSE,row.names=FALSE,append=FALSE)

if (gender=="Female"){
	row.train2 <- read.table("../dataset/test_female.txt")[,1]
}else{
	row.train2 <- read.table("../dataset/test_male.txt")[,1]
}



Dev<-rbind(Sub[row.train2,])
Test<-rbind(Sub[-row.train2,])

#summary(Test[,])


#### training cut ####

#Up<-summary(Train[,1])[5]+1.5*(summary(Train[,1])[5]-summary(Train[,1])[2])
#Lo<-summary(Train[,1])[2]-1.5*(summary(Train[,1])[5]-summary(Train[,1])[2])


#Train<-Train[Train[,1]<=Up,]
#Train<-Train[Train[,1]>=Lo,]


## over sampling ##

So <- SmoteRegress(Label~., Train, rel = "auto", thr.rel = 0.50 ,C.perc = "balance", k = 3, repl = FALSE, dist = "HEOM")
#hist(So[,1])

#Train<-rbind(T[row.train,])
Train<-rbind(Train,So)
#Train<-rbind(T[row.train,])

## RandomForest ##

best=0
for (i in c(10,20,30,40,50,60,70)){
	for (j in c(100,200,300,400,500)){

		model = randomForest(Label~., data = Train,na.action = na.roughfix,mtry=i,ntree=j)
		predict_rf<-predict(model,Dev)
		COR<-cor(predict_rf,Dev[,Lab],method="spearman",use = "complete.obs")
		#RMSE<-rmse(predict_rf,Test[,Lab])
		if (COR > best){
				best=COR
				best_i=i
				best_j=j
		}
}}


model = randomForest(Label~., data = Train,na.action = na.roughfix,mtry=best_i,ntree=best_j)

#classwt=c(10,1,100)
#model = randomForest(Label~., data = T2,,na.action = na.roughfix,mtry=300,ntree=200)
predict_rf<-predict(model,Test)

cor.test(predict_rf,Test[,Lab],method="spearman",use = "complete.obs")
Cor0<-cor(predict_rf,Test[,Lab],method="spearman",use = "complete.obs")

rmse(predict_rf,Test[,Lab])


#NUM = test
#Best = Inp
#graph_title=paste(gender," ",analysis," ","(Top:"," ",NUM,")",sep="")
#graph_name=paste(gender,"_",analysis,".png",sep="")
#png(graph_name)
#plot(predict_rf,Test[,Lab],xlab="Predicted values",ylab="Actual values",xlim=c(MIN,MAX),ylim=c(MIN,MAX),cex=1.5,cex.lab=1.3,cex.axis=1.3,main="Male - total",pch = 16)
#leg=paste("rho=",round(Inp, digits = 2),sep="")
#legend("bottomright", legend = leg,bty="n",cex=1.5)
#dev.off()
		
plot(predict_rf,Test[,Lab],xlim=c(1800,5000),ylim=c(1800,5000),xlab="Predicted values",ylab="Actual values",,cex=1.5,cex.lab=1.3,cex.axis=1.3,main="Male - total",pch = 16)

leg=paste("rho=",round(0.65, digits = 2),sep="")
legend("bottomright", legend = leg,bty="n",cex=1.5)

#plot(predict_rf,Test[,Lab],cex=1.5,main="random forest")
#plot(predict_rf,Test[,Lab],xlab="Predicted values",ylab="Actual values",xlim=c(MIN,MAX),ylim=c(MIN,MAX),cex=1.5,cex.lab=1.3,cex.axis=1.3,main=graph_title,pch = 16)

# save and read the model

inp<-paste("../model/", gender,"_",analysis,"_",Num,".obj",sep="")
saveRDS(model, file = inp)
#model_read <- readRDS("RF_1.obj")

###################################

#model$importance
#sort(model$importance)
#ORDER<-order(model$importance,decreasing=TRUE)

#OUT_FE<-colnames(T)[c(order(model$importance,decreasing=TRUE)[0:15])]

#Fea_out=paste("../random/",number,"/",gender,"_",analysis,"_top15.txt",sep="")

#write.table(OUT_FE, Fea_out, quote=FALSE, col.names=FALSE,row.names=FALSE,append=FALSE)


