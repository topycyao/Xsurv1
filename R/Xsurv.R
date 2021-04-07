#main function
#'
#' This function can fit the survival data with different model including xgboost,lightgbm,random forests, and gbm.
#' @param datax X data set
#' @param datay Y data set including time and event status
#' @param top_n number of top features for survival tree fitting(avaliable for xgb and lgb)
#' @param option model fitting option,defaut is xgb
#' @param method fitting metohd,defaut is 'pl' means using loss function:coxph likelihood
#' @param nfolds number of folds for crossvalidation
#' @param number of rounds
#' @param lambda l1 penalty parameter
#' @param alpha l2 penalty parameter
#' @param eta learning rate
#' @param early_stopping_rounds force a stopping round
#' @param gtree number of trees using in gbm model
#' @param ncores number of cores using in gbm
#' @param gfrac bag fraction in gbm with defaut 0.5
#' @param gsh shrinkage paramter in gbm with defaut 0.001
#' @param rfnsp number of splits in random forests
#' @export
#' @examples
#' sim_surv_xgb_tree(model,x_data,y_data)

Xsurv<-function(datax,datay,top_n=NULL,option=c('defaut','xgb','lgb','gbm','rf'),method=c('defaut','pl','C'),nfolds=5,
                nround=NULL,lambda=NULL,alpha=NULL,eta=NULL,early_stopping_rounds=NULL,gtree=NULL,
                ncores=NULL,gfrac=NULL,gsh=NULL,rfnsp=NULL)
{

  x_train=datax
  y=datay
  d_train <- as.data.frame(x_train)
  y_train=y$time
  d_train %<>% dplyr::mutate(yy = y)
  option=match.arg(option)
  method=match.arg(method)
  sp_tree<-NULL

  if(is.null(lambda))
    lambda=.01
  if(is.null(alpha))
    alpha=.01
  if(is.null(eta))
    eta=.01
  if(is.null(nround))
    nround=1000
  if(is.null(early_stopping_rounds))
    early_stopping_rounds=20
  if(is.null(gtree))
    gtree=1000
  if(is.null(ncores))
    ncores=1
  if(is.null(gfrac))
    gfrac=0.5
  if(is.null(gsh))
    gsh=0.001
  if(is.null(rfnsp))
    rfnsp=10
  tt<-length(x_train[,1])
  y_train_boost <-  2 * y$time * (y$status - .5) #make fisrt col status and second col time
  #y_train<-surv_time
  if(option=='lgb'){
    LDtrain <- lgb.Dataset(x_train, label = y_train_boost)
    if(method=='C')
      model<-lightgbm::lgb.cv(list(objective = cidx_lgb_obj,
                                   eta = eta, lambda = lambda, alpha = alpha, subsample = .5,
                                   colsample_bytree = .5), LDtrain, nround = nround,eval=cidx_lgb_func,
                      nfold = nfolds, verbose = 0, early_stopping_rounds = early_stopping_rounds)
    else    model<-lightgbm::lgb.cv(list(objective = Cox_lgb_obj,
                            eta = eta, lambda = lambda, alpha = alpha, subsample = .5,
                          colsample_bytree = .5), LDtrain, nround = nround,eval=cidx_lgb_func,
                         nfold = nfolds, verbose = 0, early_stopping_rounds = early_stopping_rounds)


    y_lgcox_pred <- matrix(0, tt, nfolds)
    for (i in 1:nfolds) {
      y_lgcox_pred[, i] <- -predict(model$boosters[[i]]$booster, x_train)

    }
    lgbcx<-rep(0,nfolds)
    for(i in 1:nfolds){
      aa=(as.matrix(y_lgcox_pred[,i]))
      lgbcx[i]<-survival::concordance(y_train ~ aa)$con


    }
    k=which.max(lgbcx)
    mod<-model$boosters[[k]]$booster
    cdx<-lgbcx[k]
    sp_tree<-sim_surv_lgb_tree(mod,x_train,datay,top_n)

    } else if(option=='gbm'){

    mod <- gbm::gbm(yy ~ .,       # formula
                data = d_train,                 # dataset
                distribution = "coxph",
                n.trees =gtree ,              # number of trees
                shrinkage = gsh,           # shrinkage or learning rate, 0.001 to 0.1 usually work
                bag.fraction = gfrac,        # subsampling fraction, 0.5 is probably best
                train.fraction = 0.8,      # fraction of data for training, first train.fraction*N used for training
                cv.folds = nfolds,              # do 5-fold cross-validation
                keep.data = T,
                verbose = T,
                n.cores = 1
    )

    y_gbm_predict <- exp(-predict(mod, newdata = d_train))
    cdx<-survival::concordance(y_train~y_gbm_predict)$con

  } else if(option=='rf'){
    mod <- randomForestSRC::rfsrc(yy ~ ., data = d_train, nsplit = rfnsp,importance = TRUE)
    y_rf_predict <- predict(mod, d_train)$regrOutput$yy$predicted
    cdx<-survival::concordance(y_train ~ y_rf_predict)$con

  } else  {
    XDtrain <- xgboost::xgb.DMatrix(x_train, label = y_train_boost)
    if(method=='C')
      model<-xgboost::xgb.cv(list(objective = cidx_xgb_obj, eval_metric = cidx_xgb_func,
                                  tree_method = 'hist', grow_policy = 'lossguide',
                                  eta = eta, lambda = lambda, alpha = alpha, subsample = .5,
                                  colsample_bytree = .5), XDtrain, nround = nround,
                             nfold = nfolds, verbose = F, early_stopping_rounds = early_stopping_rounds, maximize = T,
                             callbacks = list(cb.cv.predict(T)))
    else    model<-xgboost::xgb.cv(list(objective = 'survival:cox', eval_metric = cidx_xgb_func,
                                        tree_method = 'hist', grow_policy = 'lossguide',
                                        eta = eta, lambda = lambda, alpha = alpha, subsample = .5,
                                        colsample_bytree = .5), XDtrain, nround = nround,
                                   nfold = nfolds, verbose = F, early_stopping_rounds = early_stopping_rounds, maximize = T,
                                   callbacks = list(cb.cv.predict(T)))

    y_xgbcox_pred <- matrix(0, tt, nfolds)
    for (i in seq_len(5)) {
      y_xgbcox_pred[, i] <- -predict(model$models[[i]], x_train)

    }
    xgbcx<-rep(0,nfolds)

    for(i in 1:5){
      aa=(as.matrix(y_xgbcox_pred[,i]))
      xgbcx[i]<-survival::concordance(y_train ~ aa)$con

    }
    k=which.max(xgbcx)
    mod<-model$models[[k]]
      cdx<-lgbcx[k]
    sp_tree<-sim_surv_xgb_tree(mod,x_train,datay,top_n)
  }

    ls<-list('model'=mod,'cindex'=cdx,'tree'=sp_tree)
    ls
}



