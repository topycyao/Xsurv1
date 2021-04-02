#' Simple survival tree lgb
#'
#' This function allows you to find risk level automatically with lgbcv object.
#' @param model lightgbm model object
#' @param x_data X data set
#' @param y_data Y data set


#' @export
#' @examples
#' sim_surv_lgb_tree(model,x_data,y_data)
   sim_surv_lgb_tree<-function(model,x_data,y_data){
     cnames<-colnames(x_data)
     imp<-lightgbm::lgb.importance(model,percentage = TRUE)
     top_3<-imp[1:3,]
     top_3<-as.matrix(top_3)
     top_3<-as.vector(top_3)
     idx1<-which(cnames==top_3[1])
     idx2<-which(cnames==top_3[2])
     idx3<-which(cnames==top_3[3])
     idx<-c(idx1,idx2,idx3)
     x_tree<-x_data[,idx]
     tree_data<-cbind(x_data,y_data)
     n_tree<-colnames(x_tree)
     yt<-survival::Surv(y_data$time,y_data$status)
     fit<-partykit::ctree(yt~.,data=tree_data)
     plot(fit)
     fit
   }

#' Simple survival tree xgb
#'
#' This function allows you to find risk level automatically with lgbcv object.
#' @param model xgboot model object
#' @param x_data X data set
#' @param y_data Y data set
#' @export
#' @examples
#' sim_surv_xgb_tree(model,x_data,y_data)
   sim_surv_xgb_tree<-function(model,x_data,y_data){
     cnames<-colnames(x_data)
     imp<-xgboost::xgb.importance(model,percentage = TRUE)
     top_3<-imp[1:3,]
     top_3<-as.matrix(top_3)
     top_3<-as.vector(top_3)
     idx1<-which(cnames==top_3[1])
     idx2<-which(cnames==top_3[2])
     idx3<-which(cnames==top_3[3])
     idx<-c(idx1,idx2,idx3)
     x_tree<-x_data[,idx]
     tree_data<-cbind(x_data,y_data)
     n_tree<-colnames(x_tree)
     yt<-survival::Surv(y_data$time,y_data$status)
     fit<-partykit::ctree(yt~.,data=tree_data)
     plot(fit)
     fit
   }
