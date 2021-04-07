#' Simple survival tree lgb
#'
#' This function allows you to find risk level automatically with lgbcv object.
#' @param model lightgbm model object
#' @param x_data X data set
#' @param y_data Y data set
#' @export
#' @examples
#' sim_surv_lgb_tree(model,x_data,y_data)
   sim_surv_lgb_tree<-function(model,x_data,y_data,top_n=NULL){
     cnames<-colnames(x_data)
     if(is.null(top_n)){top_n=3}
     imp<-lightgbm::lgb.importance(model,percentage = TRUE)
     top_3<-imp[1:top_n,]
     top_3<-as.matrix(top_3)
     top_3<-as.vector(top_3)
     idx=rep(0,top_n)
     for(i in 1:top_n){
       idx[i]<-which(cnames==top_3[i])
     }
     x_tree<-x_data[,idx]

     yt<-survival::Surv(y_data$time,y_data$status)
     fit<-rpart::rpart(yt~.,data=x_tree)
     tfit<-partykit::as.party(fit)
     tfit1<-partykit::as.party(fit)
     plot(tfit1)
     tfit2<-partykit::ctree(yt~.,data=x_tree)
     ls<-list('tree1'=tfit1,'tree2'=tfit2)
     ls
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
   sim_surv_xgb_tree<-function(model,x_data,y_data,top_n=NULL){
     cnames<-colnames(x_data)
     if(is.null(top_n)){top_n=3}
     imp<-xgboost::xgb.importance(cnames,model=model)
     top_3<-imp[1:top_n,]
     top_3<-as.matrix(top_3)
     top_3<-as.vector(top_3)
     idx=rep(0,top_n)
     for(i in 1:top_n){
       idx[i]<-which(cnames==top_3[i])
        }
     x_tree<-x_data[,idx]
     yt<-survival::Surv(y_data$time,y_data$status)
     fit<-rpart::rpart(yt~.,data=x_tree)
     tfit1<-partykit::as.party(fit)
     plot(tfit1)
     tfit2<-partykit::ctree(yt~.,data=x_tree)
     ls<-list('tree1'=tfit1,'tree2'=tfit2)
     ls
   }
