setwd("~/Quant Trading/sp_daily")
library(quantmod)
library(TTR)
library(xts)
library(timeDate)
library(FinCal)
library(RQuantLib)
library(lubridate)
library(caret)

tradingDayOfMonth <- function(myDate, calendar = "UnitedStates/NYSE") {
    FirstOfMonth <- as.Date(paste(year(myDate), month(myDate), "01", sep="/")) 
    days<-seq(from=FirstOfMonth,to=myDate,by=1)
    return(sum(isBusinessDay(calendar,days)))
}
tradingDaysLeftInMonth<-function(myDate){
    EOM <- getEndOfMonth(calendar = "UnitedStates/NYSE",myDate)
    days<-seq(from=myDate, to = EOM,by=1)
    return(sum(isBusinessDay("UnitedStates/NYSE",days)))
}
DaysFromLastHoliday <- function(myDate, hdays,calendar = "UnitedStates/NYSE"){
    d <- hdays[hdays<myDate]
    days<-seq(from=d[which.min(abs(myDate-d))], to = myDate,by=1)
    return(sum(isBusinessDay(calendar,days)))
}
DaysTilNextHoliday <- function(myDate, hdays,calendar = "UnitedStates/NYSE"){
    d <- hdays[hdays>myDate]
    days<-seq(from=myDate, to=d[which.min(abs(myDate-d))],by=1)
    return(sum(isBusinessDay(calendar,days)))
}
ReplwNA <- function(x, threshold){ # replace a vector with NA if value exceeds threshold
    x[x>threshold]<-NA
    return(x)
}
Event <- function(x, threshold=1.0, from="below"){
    # Return T if previous value is below (above) threshold and current value is >= (<=) 
    # the treshold when from = below (above)
    z<-matrix(rep(x,2),nrow=length(x),ncol=2)
    z[2:length(x),1]<-z[1:(length(x)-1),2]
    if (from=="below" | from=="b"){
        out<-z[,1]<threshold & z[,2]>=threshold
    } else {
        out<-z[,1]>threshold & z[,2]<=threshold
    }
    out[is.na(out)]<-FALSE    
    return(out)
}

build_data<-function(spx,spx.ret){
    data<-spx[,"Close",drop=FALSE]
    colnames(data)<-"Close"
    data$SMA2<-data$Close/SMA(data$Close,2) # Close relative to simple moving average
    data$SMA5<-data$Close/SMA(data$Close,5)
    data$SMA10<-data$Close/SMA(data$Close,10)
    data$SMA20<-data$Close/SMA(data$Close,20)
    data$SMA50<-data$Close/SMA(data$Close,50)
    data$SMA200<-data$Close/SMA(data$Close,200)
    data$SMA50_200<- data$SMA200/data$SMA50 # 50d SMA /  200d SMA
    data$BB<-BBands(data$Close)[,4] # %B of Bollinger Band
    data$wday<-factor(weekdays(index(data),TRUE),
                      levels=c("Mon","Tue","Wed","Thu","Fri")
    ) # day of week
    data$tdayofmonth<-sapply(index(data),tradingDayOfMonth) # trading day of month
    data$tdaysleft<-sapply(index(data),tradingDaysLeftInMonth) # trading day remaining in month
    data$month<-factor(month(index(data))) # month
    data$vol5<-rollapply(data.frame(spx.ret),5,sd,fill=NA) # 5 day vol
    data$vol10<-rollapply(data.frame(spx.ret),10,sd,fill=NA) #10 day vol
    data$volchg=data$vol5-data$vol10 # 5d vol - 10d vol (change in vol)
    NYSEhdays<-as.Date(holidayNYSE(year(index(data)[1]):(year(index(data)[nrow(data)])+1)))
    data$daysfromhday <- sapply(index(data),DaysFromLastHoliday,hdays=NYSEhdays)
    data$daystilhday <- sapply(index(data),DaysTilNextHoliday,hdays=NYSEhdays)
    data$SMA50_200Factor<-data[,"SMA50_200"]>=1
    data$E_SMA2xb<-Event(data$SMA2,1,"b")
    data$E_SMA2xa<-Event(data$SMA2,1,"a")
    data$E_SMA5xb<-Event(data$SMA5,1,"b")
    data$E_SMA5xa<-Event(data$SMA5,1,"a")
    data$E_SMA10xb<-Event(data$SMA10,1,"b")
    data$E_SMA10xa<-Event(data$SMA10,1,"a")
    data$E_SMA20xb<-Event(data$SMA20,1,"b")
    data$E_SMA20xa<-Event(data$SMA20,1,"a")
    data$E_SMA50xb<-Event(data$SMA50,1,"b")
    data$E_SMA50xa<-Event(data$SMA50,1,"a")
    data$E_SMA200xb<-Event(data$SMA200,1,"b")
    data$E_SMA200xa<-Event(data$SMA200,1,"a")
    data$E_SMA50_200xb<-Event(data$SMA50_200,1,"b")
    data$E_SMA50_200xa<-Event(data$SMA50_200,1,"a")
    data$ret <- lag(spx.ret,-2)
    data<-data[complete.cases(data),]
    #saveRDS(data,"data_mdl.rds")
    return(data)
}
build_data2<-function(spx,spx.ret){
    close<-spx$Close
    data<-close/SMA(close,2) # Close relative to simple moving average
    colnames(data)<-"SMA2"
    data$SMA5<-close/SMA(close,5)
    data$SMA10<-close/SMA(close,10)
    data$SMA20<-close/SMA(close,20)
    data$SMA50<-close/SMA(close,50)
    data$SMA200<-close/SMA(close,200)
    data$SMA50_200<- data$SMA200/data$SMA50 # 50d SMA /  200d SMA
    data$BB<-BBands(close)[,4] # %B of Bollinger Band
    data$wday<-factor(weekdays(index(data),TRUE),
                      levels=c("Mon","Tue","Wed","Thu","Fri")
    ) # day of week
    data$tdayofmonth<-sapply(index(data),tradingDayOfMonth) # trading day of month
    data$tdaysleft<-sapply(index(data),tradingDaysLeftInMonth) # trading day remaining in month
    data$month<-factor(month(index(data))) # month
    data$vol5<-rollapply(spx.ret,width=5,sd) # 5 day vol
    data$vol10<-rollapply(spx.ret,width=10,sd) #10 day vol
    data$volchg=data$vol5-data$vol10 # 5d vol - 10d vol (change in vol)
    NYSEhdays<-as.Date(holidayNYSE(year(index(data)[1]):(year(index(data)[nrow(data)])+1)))
    data$daysfromhday <- sapply(index(data),DaysFromLastHoliday,hdays=NYSEhdays)
    data$daystilhday <- sapply(index(data),DaysTilNextHoliday,hdays=NYSEhdays)
    data$SMA50_200Factor<-data[,"SMA50_200"]>=1
    data$E_SMA2xb<-Event(data$SMA2,1,"b")
    data$E_SMA2xa<-Event(data$SMA2,1,"a")
    data$E_SMA5xb<-Event(data$SMA5,1,"b")
    data$E_SMA5xa<-Event(data$SMA5,1,"a")
    data$ret <- lag(spx.ret,-2)
    data<-data[complete.cases(data),]
}
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
evaltestc<-function(mdl,testdata,yact){
    pred<-predict(mdl,newdata=testdata) #test set
    pred_neg_idx<-pred==TRUE
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


spx<-readRDS("spx.rds")
spx.ret<-dailyReturn(spx$Close,leading=FALSE)*100

data<-build_data2(spx,spx.ret)
data<-data.frame(data)
#data$wday<-factor(data$wday)
#data$month<-factor(data$month)

#train.idx<-row.names(data)<"1982-10-18" # 32 years
train.idx<-seq(1:floor(nrow(data)/2))
train<-data[train.idx,]
test<-data[-train.idx,]

train_df<-data.frame(train[,colnames(train)!="ret"]) #ignore ret
test_df<-data.frame(test[,colnames(test)!="ret"]) #ignore ret
ytrain<-as.vector(train[,"ret"])
ytrain_class<-factor(ytrain<0)
ytest<-as.vector(test[,"ret"])
ytest_class<-factor(ytest<0)

library(doParallel)
cl<-makeCluster(3)
registerDoParallel(cl)
library(caret)


rf_raw<-train(train_df,ytrain,method="rf")
#rfc_raw<-train(train_df,ytrain_class,method="rf")

rf_raw_eval<-evaltest(rf_raw,test_df,ytest)
#rfc_raw_eval<-evaltestc(rfc_raw,test_df,ytest_class)
print("rf_raw")
print(rf_raw_eval$summary)
print(rf_raw_eval$strategy)
print(rf_raw_eval$bh)

#print("rfc_raw")
#print(rfc_raw_eval$summary)
#print(rfc_raw_eval$strategy)
#print(rfc_raw_eval$bh)