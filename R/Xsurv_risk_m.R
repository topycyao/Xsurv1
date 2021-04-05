
#Find survival risk with a xgboot or lightgbm model

Xsurv_risk_m<-function(model,train_data,test_data,lower,upper){

  pred_train<-as.data.frame(predict(model,train_data))
  pred_test<-as.data.frame(predict(model,test_data))
  #set upper bound and lower bound for risk level
  h_mq=quantile(exp(pred_train[,1]),upper)

  l_mq=quantile(exp(pred_train[,1]),lower)

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

  cl2<-ris_tran_tr(exp(pred_train[,1]),l_mq,h_mq)
  #center=cl2$centers
  #risklevel<-sort(center)

  #r_test<-kmeans(x_pred_cix,centers = risklevel)
  #print(r_test$size)


  #cl_pred_x2<-knn(x_pred_cix,x_pred_cix_test,cl=r_test$cluster)


  #pred_x2_risk<-ris_tran(cl_pred_x2)


  cl_pred<-class::knn(exp(pred_train),exp(pred_test),cl=cl2)



  prisk<-factor(cl_pred,levels = c('High Risk','Medium Risk','Low Risk'))




  return(prisk)


}
