#Find Risk by manully setting risk level for train data
surv_risk_m_xgbcv<-function(model,x_train,x_test){

  XDtest <- xgboost::xgb.DMatrix(x_test, label = y_test_boost)
  x_pred_cox <- as.data.frame(-rowMeans(sapply(model$models, predict, x_train)))


  y_xgcox_predict<--rowMeans(sapply(model$models, predict, x_test))

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



  cl_pred_x2<-class::knn(exp(-x_pred_cox),exp(-x_pred_cox_test),cl=cl2)


  #pfx2<-factor(pred_x2_risk,levels = c('High Risk','Medium Risk','Low Risk'))
  prisk_x2<-factor(cl_pred_x2,levels = c('High Risk','Medium Risk','Low Risk'))



  return(prisk_)
}
