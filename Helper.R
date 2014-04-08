###########################
##we are helper functions##
###########################

### READ the data
saveData<-function(){
  loan<-as.data.frame(fread("../data/train_v2.csv"),header=TRUE,sep=",")
  print('fill the missing value')
  med <- apply(loan, 2, median, na.rm=TRUE)
  loan <- Impute(loan, med)
  
  #### some features
  # the default status
  loan$default <- ifelse(loan$loss>0, 1, 0)
  loan$f274_f527 <- loan$f274 - loan$f527
  loan$f274_f528 <- loan$f274 - loan$f528
  loan$f527_f528 <- loan$f527 - loan$f528
  loan$logf271<-(log(1+loan$f271))
  ### Do the scaling
  sigma<-apply(loan,2,sd)
  average<-apply(loan,2,mean)
  print('scale the train_v2...')
  name<-colnames(loan)
  m<-ncol(loan)
  for(i in 1:ncol(loan)){
    print(i)
    if( (name!='id') & (name!='loss') & (name!='default') ){
      loan[,i]<-(loan[,i]-average[[i]])/sigma[[i]]
    }
  }
  save(loan,file='loan_scale.Rdata')
  rm(loan)
  
  
  test <- as.data.frame(fread("../data/test_v2.csv", header=TRUE, sep=","))
  test <- Impute(test, med[1:dim(test)[2]])
  
  test$f274_f527 <- test$f274 - test$f527
  test$f274_f528 <- test$f274 - test$f528
  test$f527_f528 <- test$f527 - test$f528
  test$logf271<-log(1+test$f271)
  ### Do the scaling
  for(i in 1:m){
    print(i)
    if((name!='id') & (name!='loss') & (name!='default') ){
      test[,name[i]]<-(test[,name[i]]-average[[i]])/sigma[[i]]
    }
  }  
  save(test,file='test_scale.Rdata')
  rm(test)
}

F1test<-function(pred,obs){
  true_pos<-sum(obs == 1)
  pred_pos<-sum(pred == 1)
  TP<-sum(obs==1 & pred==1)
  precision<-TP/pred_pos
  recall<-TP/true_pos
  f1_score<-2*precision*recall/(precision+recall)
  cat('precision',precision,'\n')
  cat('recall',recall,'\n')
  cat('F1 score',f1_score,'\n')
  return(ifelse(is.na(f1_score),0,f1_score))  #預防有pred_pos或true_pos=0的情形
}

Impute <- function(data, value){
  
  num <- apply(data, 2, function(x) sum(is.na(x)))
  
  data <- as.matrix(data)
  data[which(is.na(data))] <- rep(value, num)
  data <- as.data.frame(data)
  
  return(data)
}



trainDefaulterClassifier <- function(loan, gbm_params, plot_on=FALSE){
  gc(reset=TRUE)
  gbm_flag<-T
  if(gbm_flag==T){
    model <- gbm(
      default ~ .,
      data = loan,
      distribution = 'bernoulli',
      n.trees = gbm_params$n.trees,
      shrinkage = gbm_params$shrinkage,
      interaction.depth = gbm_params$interaction.depth,
      train.fraction = gbm_params$train.fraction,
      bag.fraction = gbm_params$bag.fraction,
      n.minobsinnode = gbm_params$n.minobsinnode,
      cv.folds = gbm_params$cv.folds,
      class.stratify.cv = gbm_params$class.stratify.cv,
      verbose = gbm_params$verbose,
      n.cores = gbm_params$n.cores,
      keep.data = gbm_params$keep.data)
  }else{
    model<-glm(default~.,data=loan,family=binomial)
  }
  if(plot_on == TRUE){
    par(mfrow=c(1,1))
    gbm.perf(model)
    min.cv.err <- min(model$cv.err, na.rm=T)
    title(main=paste('Minimum cv.err = ', round(min.cv.err, 5), sep=''))
    x <- seq(-gbm_params$n.trees, 2*gbm_params$n.trees)
    y <- rep(min.cv.err, length(x))
    points(x, y, type='l', col='blue', lwd=2, lty=2)
  }
  return(model)
}