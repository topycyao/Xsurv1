library(class)
library(MASS)
library(xgboost)
library(survival)
library(dplyr)

library(magrittr)
library(survcomp)
library(data.table)

####manully set risk level for train data
surv_risk_m_xgbcv<-function(model,x_train,y_train,x_test,y_test){
  #convert data to xgb data

  y_train_boost <-  2 * y_train$time * (y_train$status - .5)
  y_test_boost <-  2 * y_test$time * (y_test$status - .5)
  XDtrain <- xgb.DMatrix(x_train, label = y_train_boost)
  XDtest <- xgb.DMatrix(x_test, label = y_test_boost)
  x_pred_cox <- as.data.frame(-rowMeans(sapply(model$models, predict, XDtrain)))


  y_xgcox_predict<--rowMeans(sapply(model$models, predict, XDtest))

  x_pred_cox_test <- as.data.frame(y_xgcox_predict)

  ris_tran_tr <- function(x,a,b) {
    k<-length(x)
    y<-rep(0,k)
    x<-as.numeric(x)
    for (i in 1:k) {
      if(x[i]<=a) {y[i]<-'Low Risk'}
      else if(x[i]>=b) {y[i]<-'High Risk'}
      else y[i]<-'Medium Risk'
    }



    return(y)
  }

  h_mq=quantile(exp(-x_pred_cox[,1]),upper)

  l_mq=quantile(exp(-x_pred_cox[,1]),lower)


  cl2<-ris_tran_tr(exp(-x_pred_cox[,1]),l_mq,h_mq)



  cl_pred_x2<-knn(exp(-x_pred_cox),exp(-x_pred_cox_test),cl=cl2)


  #pfx2<-factor(pred_x2_risk,levels = c('High Risk','Medium Risk','Low Risk'))
  prisk_x2<-factor(cl_pred_x2,levels = c('High Risk','Medium Risk','Low Risk'))



  return(prisk_)
}
