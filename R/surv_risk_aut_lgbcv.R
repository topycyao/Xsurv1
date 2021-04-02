#' Fit survival risk
#'
#' This function allows you to find risk level automatically with lgbcv object.
#' @param model Model to be applied
#' @param x_train Xtrain data set
#' @param y_trainl Ytrain data set
#' @param x_test Xtest data set
#' @param y_test Ytest data set
#' @keywords Survival Risk
#' @export
#' @examples
#' surv_risk_aut_lgbcv()
surv_risk_aut_lgbcv<-function(model,x_train,y_train,x_test,y_test){



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


  cl3=kmeans(l_pred_cox,3)
  cl3$size
  k=length(cl3$centers)
  risklevel=rep(0,length(x_test[,1]))
  center=cl3$centers
  risklevel<-sort(center)

  r_test<-kmeans(l_pred_cox,centers = risklevel)
  print(r_test$size)







  cl_pred_l1<-class::knn(l_pred_cox,l_pred_cox_test,cl=r_test$cluster)


  pred_l1_risk<-ris_tran(cl_pred_l1)
  pfl1<-factor(pred_l1_risk,levels = c('High Risk','Medium Risk','Low Risk'))
  pfl1
}

