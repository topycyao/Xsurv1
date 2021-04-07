#Find risk automatically with average prediction from xgb.cv object

surv_risk_aut_xgbcv<-function(model,x_train,x_test){
  x_train<-data.matrix(x_train)
  x_test<-data.matrix(x_test)

  x_pred_cox <- as.data.frame(-rowMeans(sapply(model$models, stats::predict, x_train)))


  y_xgcox_predict<--rowMeans(sapply(model$models, stats::predict, x_test))

  x_pred_cox_test <- as.data.frame(y_xgcox_predict)

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


  cl1=kmeans(x_pred_cox,3)


  center=cl1$centers
  risklevel<-sort(center)

  r_test<-kmeans(x_pred_cox,centers = risklevel)
  print(r_test$size)


  cl_pred_x<-class::knn(x_pred_cox,x_pred_cox_test,cl=r_test$cluster)


  pred_x_risk<-ris_tran(cl_pred_x)


  prisk_x<-factor(pred_x_risk,levels = c('High Risk','Medium Risk','Low Risk'))
  prisk_x
}
