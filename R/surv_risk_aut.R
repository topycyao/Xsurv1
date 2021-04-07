

###define risk automatically
surv_risk_aut<-function(model,train_data,test_data){
  train_data<-data.matrix(train_data)
  test_data<-data.matrix(test_data)
  pred_train<-as.data.frame(-predict(model,train_data))
  pred_test<-as.data.frame(-predict(model,test_data))
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
