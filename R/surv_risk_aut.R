

###define risk automatically
surv_risk_aut<-function(model,train_data,test_data){
  train_data<-data.matrix(train_data)
  test_data<-data.matrix(test_data)
  pred_train<-as.data.frame(-stats::predict(model,train_data))
  pred_test<-as.data.frame(-stats::predict(model,test_data))

  cl1=stats::kmeans(pred_train,3)



  center=cl1$centers
  risklevel<-sort(center)

  r_test<-stats::kmeans(pred_train,centers = risklevel)
  ris_tran <- function(x) {
    k<-length(x)
    y<-rep(0,k)
    x<-as.numeric(x)
    for (i in 1:k) {
      if(x[i]==1) {y[i]<-'High Risk'}
      else if(x[i]==2) {y[i]<-'Medium Risk'}
      else y[i]<-'Low Risk'
    }



    return(y)
  }
  pred_train_lf<-leaf_tf(model,train_data)
  pred_test_lf<-leaf_tf(model,test_data)
  pred_train_lf<-as.data.frame(pred_train_lf)
  pred_test_lf<-as.data.frame(pred_test_lf)
  cl_pred<-class::knn(pred_train_lf,pred_test_lf,cl=r_test$cluster)


  pred_risk<-ris_tran(cl_pred)


  prisk<-factor(pred_risk,levels = c('High Risk','Medium Risk','Low Risk'))

  return(prisk)


}
#risk for gbm
surv_risk_aut_gbm<-function(model,train_data,test_data){
  train_data<-as.data.frame(train_data)
  test_data<-as.data.frame(test_data)
  pred_train<-as.data.frame(-gbm::predict.gbm(model,train_data))
  pred_test<-as.data.frame(-gbm::predict.gbm(model,test_data))
  cl1=stats::kmeans(pred_train,3)



  center=cl1$centers
  risklevel<-sort(center)

  r_test<-stats::kmeans(pred_train,centers = risklevel)
  ris_tran <- function(x) {
    k<-length(x)
    y<-rep(0,k)
    x<-as.numeric(x)
    for (i in 1:k) {
      if(x[i]==1) {y[i]<-'High Risk'}
      else if(x[i]==2) {y[i]<-'Medium Risk'}
      else y[i]<-'Low Risk'
    }



    return(y)
  }



  cl_pred<-class::knn(pred_train,pred_test,cl=r_test$cluster)


  pred_risk<-ris_tran(cl_pred)


  prisk<-factor(pred_risk,levels = c('High Risk','Medium Risk','Low Risk'))

  return(prisk)


}
##risk for rf
surv_risk_aut_rf<-function(model,train_data,test_data){
  train_data<-as.data.frame(train_data)
  test_data<-as.data.frame(test_data)
  pred_train<-as.data.frame(randomForestSRC::predict.rfsrc(mod, train_data)$regrOutput$yy$predicted)
  pred_test<-as.data.frame(randomForestSRC::predict.rfsrc(mod, test_data)$regrOutput$yy$predicted)
  cl1=stats::kmeans(pred_train,3)



  center=cl1$centers
  risklevel<-sort(center)

  r_test<-stats::kmeans(pred_train,centers = risklevel)
  ris_tran <- function(x) {
    k<-length(x)
    y<-rep(0,k)
    x<-as.numeric(x)
    for (i in 1:k) {
      if(x[i]==1) {y[i]<-'High Risk'}
      else if(x[i]==2) {y[i]<-'Medium Risk'}
      else y[i]<-'Low Risk'
    }



    return(y)
  }



  cl_pred<-class::knn(pred_train,pred_test,cl=r_test$cluster)


  pred_risk<-ris_tran(cl_pred)


  prisk<-factor(pred_risk,levels = c('High Risk','Medium Risk','Low Risk'))

  return(prisk)


}


