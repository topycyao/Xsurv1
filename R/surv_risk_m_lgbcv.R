#risk level
####

##define risk manually

###define risk automatically with average prediction from xgb.cv object



###define risk automatically with average prediction from lgb.cv object
library(class)
library(MASS)
library(lightgbm)
library(survival)
library(dplyr)

library(magrittr)
library(survcomp)
library(data.table)
###
surv_risk_m_lgbcv<-function(model,x_train,y_train,x_test,y_test){
  #convert data to xgb data

  y_train_boost <-  2 * y_train$time * (y_train$status - .5)
  y_test_boost <-  2 * y_test$time * (y_test$status - .5)
  tt<-length(y_train_boost)
  tt2<-length(y_test_boost)
  y_lgcox_pred_train  <- matrix(0, tt, 5)
  for (i in seq_len(5)) {
    y_lgcox_pred_train[, i] <- predict(model$boosters[[i]]$booster, x_train)

  }

  y_lgcox_pred_test  <- matrix(0, tt2, 5)
  for (i in seq_len(5)) {
    y_lgcox_pred_test[, i] <- predict(model$boosters[[i]]$booster, x_test)

  }




  l_pred_cox <- as.data.frame(-rowMeans(y_lgcox_pred_train))
  l_pred_cix <- as.data.frame(-rowMeans(y_lgcidx_pred_train))

  y_lgcox_predict<--rowMeans(y_lgcox_pred_test)

  l_pred_cox_test <-as.data.frame(y_lgcox_predict)

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

  h_mq=quantile(exp(-l_pred_cox[,1]),upper)

  l_mq=quantile(exp(-l_pred_cox[,1]),lower)




  cl4<-ris_tran_tr(exp(-l_pred_cox[,1]),l_mq,h_mq)






  cl_pred_l2<-knn((-l_pred_cox),(-l_pred_cox_test),cl=cl4)


  pfl2<-factor(cl_pred_l2,levels = c('High Risk','Medium Risk','Low Risk'))


  pfl2
}

