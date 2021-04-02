#Survival time prediction
#'
#' This function allows you to transform survival prediction from risk score to survival time.
#' @param model xgboost or lightgbm model object
#' @param x_train X train data set
#' @param y_train Y trian data set
#' @param x_test X test data set
#' @param nc number of cutting points defaut is 25, and 20-30 is recommended
#' @param nq quantile of train data time in each cut group for prediction time,defaut is and 0.9.0.8-0.95 is recommended.
#' @export
#' @examples
#' xsurv_predict(model,x_train,y_train,x_test)

xsurv_predict<-function(model,x_train,y_train,x_test,nc=25,nq=0.9)
{

  l_pred=as.data.frame(-predict(model,x_train))
  l_pred_test=as.data.frame(-predict(model,x_test))

  cl25=kmeans(l_pred,nc)
  cl25$cluster

  k=length(cl25$centers)

  center=cl25$centers
  risklevel<-sort(center)

  pre_test<-kmeans(l_pred,centers = risklevel)
  time_train<-as.data.frame(y_train$time)

  time_train$clus<-pre_test$cluster


  pre_cl<-rep(0,nc)
  for(j in 1:nc)
  {
    ind<-which(time_train[,2]==j)

    #censored data included,hence nq should be something over 0.5,defaut is 0.9
    pre_cl[j]=quantile(time_train[ind,2],nq)

  }
  # pre_adj<-sort(pre_cl)


  kpre_test=class::knn(l_pred,l_pred_test,cl = pre_test$cluster)





  surv_pred<-pre_cl[kpre_test]


  d_pred<-data.frame(time=surv_pred)

  d_pred

}
