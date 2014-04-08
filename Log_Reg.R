require(data.table)
require(bit64)
require(caret)
require(MASS)
require(gbm)
require(e1071)
#####initialize####
#
scale= TRUE
if(scale){
  load('loan_scale.Rdata')
}else{
  load('loan.Rdata')
}

load('Default_features.RData')
ChengLong<-default_predictors[1:15]
goden<-c('f274','f527','f528')
golden_diff<-c('f274_f527','f527_f528','f274_f528')
mochi<-c('f271','f272','f274','f527','f528')
dmc_RF<-c('f777', 'f2', 'f527_f528', 'f274_f528', 'f222', 'logf271')
dmc_GBM<-c('f777', 'f2', 'f527_f528', 'f274_f528', 'f222', 'f68')
default_predictors<-golden

loan<-loan[,c(default_predictors,'default')]

ratio=0.7
split<-createDataPartition(loan$default, p=ratio)[[1]]
training<-loan[split,]
testing<-loan[-split,]


###Logistic Regression (Choose the best threshold)
cat('Training...\n')
model<-glm(default~.,data=training,family=binomial)
pred_origin<-predict(model,testing[,default_predictors])
View(pred_origin)
bestF1<-0
bestTr<-0

for(tr in seq(-10,2, 0.01) ){
  pred<-pred_origin
  pred[pred_origin>tr]<-1
  pred[pred_origin<=tr]<-0
  F1score<-F1test(pred,testing$default)
  if(F1score>bestF1){
    bestTr<-tr
    bestF1<-F1score
  }
  #cat('threshold: ',tr,'\n')
  #cat('F1 score:',F1score,'\n')
  #print('===================================')
}
print(bestTr)
print(bestF1)


####Retrain####
rm(training,testing)
model<-glm(default~.,data=loan,family=binomial) ##
rm(loan)
if(scale){
  load('loan_scale.Rdata') 
}else{
  load('loan.Rdata')
}
train<-loan
rm(loan)
load('mochi_Reg_features.RData')
pred_train<-predict(model,train[,default_predictors])
train<-train[pred_train>bestTr,c(loss_predictors,'loss')]
train<-train[,-which(colnames(train)=='f382')]
print(nrow(train))
model_second<-svm(loss~.,data=train,type='eps-regression')
rm(train)


#########Predict
if(scale){
  load('test_scale.Rdata')
}else{
  load('test.Rdata')
}
pred<-predict(model,test[,default_predictors])
result<-pred
result[result>bestTr]<-predict(model_second,test[result>bestTr,loss_predictors])
result[result<=bestTr]<-0
result[result<0]<-0
result[result>100]<-100
View(result)
id<-as.integer(test$id)
output<-as.data.frame(id)
output$loss<-result
write.csv(output,'407scale_golden.csv',row.names=F,quote=F)
