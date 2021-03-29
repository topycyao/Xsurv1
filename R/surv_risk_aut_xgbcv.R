library(class)
library(MASS)
library(xgboost)
library(survival)
library(dplyr)

library(magrittr)
library(survcomp)
library(data.table)

surv_risk_aut_xgbcv<-function(model,x_train,y_train,x_test,y_test){
  #convert data to xgb data

  y_train_boost <-  2 * y_train$time * (y_train$status - .5)
  y_test_boost <-  2 * y_test$time * (y_test$status - .5)
  XDtrain <- xgb.DMatrix(x_train, label = y_train_boost)
  XDtest <- xgb.DMatrix(x_test, label = y_test_boost)
  x_pred_cox <- as.data.frame(-rowMeans(sapply(model$models, predict, XDtrain)))


  y_xgcox_predict<--rowMeans(sapply(model$models, predict, XDtest))

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


  cl_pred_x<-knn(x_pred_cox,x_pred_cox_test,cl=r_test$cluster)


  pred_x_risk<-ris_tran(cl_pred_x)


  prisk_x<-factor(pred_x_risk,levels = c('High Risk','Medium Risk','Low Risk'))
  prisk_x
}
