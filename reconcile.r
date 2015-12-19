setwd("~/Quant Trading/sp_daily")
library(quantmod)
library(TTR)
library(xts)
library(timeDate)
library(FinCal)
library(RQuantLib)
library(lubridate)

spx<-readRDS("spx.rds")
spx.ret<-dailyReturn(spx$Close,leading=FALSE)*100

# PART 1: REPRODUCING ORIGINAL
# First look at data. Get data stored from original
data1<-readRDS("data_mdl.rds") #stored from original

# Try to recreate data
source("SP500_model.r")
data2<-createdata(spx,spx.ret)

if (sum(data1!=data2)==0){
    print("data1 matches data2")
} else {
    print("data1 does NOT match data2")
}

data<-data2 #if they match it doesn't matter which we use.
# Compare RF models
rf1<-readRDS("rf1.rds") # load RF model
# Split data into training and test
train.idx<-seq(1:floor(nrow(data)/2))
train<-data[train.idx,]
test<-data[-train.idx,]

# Rebuild rf model
library(doParallel)
cl<-makeCluster(3)
registerDoParallel(cl)
library(caret)
# Admission of idiocy.  Did not set seed in original, so random forests may be different.
# specifying the mtry parameter from rf1 to reduce speed and increate prob of match
rf2<-train(ret~.,data = train,method="rf",tuneGrid=data.frame(.mtry=rf1$finalModel$mtry))

# note the similarity of rf1 with mtry=17 which is the value used in rf1
rf1$finalModel$mtry
rf1
rf2

# since are not exact, let's see how close the values they predict are
pred1<-predict(rf1,newdata=test)
pred2<-predict(rf2,newdata=test)
# we'll look at summary statistics, and correlation
summary(pred1)
summary(pred2)
cor(pred1,pred2)
plot(pred1,pred2,main="Predictions from RF1 v RF2",xlab="PRED1",ylab="PRED2")

# the RF1 has a higher R2 and lower RMSE
yact<-test[,"ret"]
R2(pred1,yact)
R2(pred2,yact)
RMSE(pred1,yact)
RMSE(pred2,yact)

# But for the real test, out of sample performance.
# it's different but still good and close enough
evaltest<-function(mdl,testdata,yact){
    pred<-predict(mdl,newdata=testdata) #test set
    pred_neg_idx<-pred<0
    pred_neg<-yact[pred_neg_idx]
    pred_pos<-yact[!pred_neg_idx]
    summary_test<-rbind(summary(pred_neg),summary(pred_pos),summary(yact))
    pred_neg_neg<-round(100*sum(pred_neg<0)/length(pred_neg),1)
    pred_pos_neg<-round(100*sum(pred_pos<0)/length(pred_pos),1) 
    act_neg<-round(100*sum(yact<0)/length(yact),1)
    summary_test<-cbind(summary_test,rbind(pred_neg_neg,pred_pos_neg,act_neg))
    rownames(summary_test)<-c("Pred Neg","Pred Pos","All")
    colnames(summary_test)[7]<-"% Neg"
    strategy<-prod(1-pred_neg/100)*prod(1+pred_pos/100)
    bh<-prod(1+yact/100)
    out<-list()
    out$summary<-summary_test
    out$strategy<-strategy
    out$bh<-bh
    return(out)
}
eval1<-evaltest(rf1,testdata=test,yact=yact)
eval2<-evaltest(rf2,testdata=test,yact=yact)
eval1$summary
eval2$summary
eval1$strategy
eval2$strategy
eval1$bh
eval2$bh
# in terms of approx annualized rates of return %
ndays<-length(yact)
100*(eval1$strategy^(252/ndays)-1)
100*(eval2$strategy^(252/ndays)-1)
100*(eval2$bh^(252/ndays)-1)

# PART 2: A Simpler Model
# some variables were not so important.  And it's a mystery why price is. So let's
# see what happens if we eliminate them.
rf1$finalModel$importance

colnames(data)
vars<-c(seq(2,23),34)
data3<-data[,vars]
colnames(data3)

# build and test a new model with this simpler model
# worked as well or better.
train<-data3[train.idx,]
test<-data3[-train.idx,]
rf3<-train(ret~.,data = train,method="rf",tuneGrid=data.frame(.mtry=rf1$finalModel$mtry))
save(rf3,train,test,file="rf3.rdata")
eval3<-evaltest(rf3,testdata=test,yact=yact)
eval1$summary
eval2$summary
eval3$summary
eval1$strategy
eval2$strategy
eval3$strategy
eval1$bh
eval2$bh
eval3$bh
ndays<-length(yact)
100*(eval1$strategy^(252/ndays)-1)
100*(eval2$strategy^(252/ndays)-1)
100*(eval2$strategy^(252/ndays)-1)
100*(eval3$bh^(252/ndays)-1)
pred3<-predict(rf3,newdata=test)
R2(pred1,yact)
R2(pred2,yact)
R2(pred3,yact)
RMSE(pred1,yact)
RMSE(pred2,yact)
RMSE(pred3,yact)
residualValues<-yact-pred3
summary(residualValues)
axisRange<-extendrange(c(pred3,yact))
plot(pred3,yact,ylim=axisRange,xlim=axisRange,main="Actual v Predicted",
     ylab="Actual",xlab="Predicted")
abline(0,1,col="darkgray",lty=2)
plot(pred3,residualValues,ylab="Residual",xlab="Predicted",main="Residuals")
abline(h=0,col="darkgray",lty=2)
stopCluster(cl)
