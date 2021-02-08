\name{lgb.sur}
\alias{lgb.sur}

\title{
fit a survival model from LightGBM
}
\description{
\code{lgb.sur} is a function to model survival data using lightgbm::lgb.cv with
self-defined loss functions includeing cox ph partial likelihood and smoothed C-index
}
\usage{
lgb.sur(datax, datay, method = c("defaut", "pl", "C"), nfolds = 5, nround = NULL, lambda = NULL, alpha = NULL, eta = NULL, early_stopping_rounds = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datax}{
data for covariates
}
  \item{datay}{
data for survival time and events
}
  \item{method}{
choose a loss function to be used,the default is "pl".
}
  \item{nfolds}{
default is 5.
}
  \item{nround}{
default is 1000
}
  \item{lambda}{
L1 penalty parameter with default .01
}
  \item{alpha}{
L2 penalty parameter with default .01
}
  \item{eta}{
learning rate with default .01
}
  \item{early_stopping_rounds}{
early stopping rounds with default 20
}
}

\value{
an objective of class \code{lightgbm:lgb.cv}.
}

\examples{
library(MASS)
library(xgboost)
library(survival)
library(dplyr)
library(magrittr)
library(survcomp)

#simulate survival data
 n <- 1000
 p <- 100
 rho <- 0.5
 tt <- round(.8*n)
 V <- diag(p)

 X <- mvrnorm(n = n, mu = rep(0, p), Sigma = V)
 xname<-rep(0,100)
 for (i in 1:100) {
   tx<-paste("X",i,sep='')
   xname[i]<-tx

 }
 colnames(X)<-xname
# ### nonlinear transformation
 mu <- exp(2*pnorm((X[, 10] > 0.5) + X[, 20] ^ 2 - 1) +
             2*pnorm(0.5*X[, 30]+X[, 40]^2 - 1) +
             2*pnorm(0.5 * X[, 50]+X[, 60] ^ 2 - 1) +
             2*pnorm(sin(X[, 70]) + X[, 80] ^ 2 - 1) +
             2*pnorm(cos(X[, 90])+X[, 100] ^ 2 - 1))
# ### survival time simulation

 obs_time <- -(log(runif(n)))/(mu)

 a <- 2*rbinom(n = n, size = 1, prob = 1/3)
 b <- runif(n = n, min = 0, max = 2)
 a[a == 0] <- b[a == 0]
 C <- a
 obs_time <- pmin(obs_time, C)
 status <- as.numeric(obs_time <= C)
 surv_time <- Surv(obs_time, status)
 y<-cbind(status,obs_time)
 colnames(y)<-c('status','time')
 y<-as.data.frame(y)
 surv_time_boost <-  2 * obs_time * (status - .5)
 x_train <- X[seq_len(tt), ]
 y_train <- y[seq_len(tt),]

 x_test <- X[(tt + 1) : n, ]
 y_test <- surv_time[(tt + 1) : n]
 y_test_boost <- surv_time_boost[(tt + 1) : n]
 lgb_cox_m<-lgb.sur(x_train,y_train)
 lgb_cix_m<-lgb.sur(x_train,y_train,method = 'C')
 LDtest <- lgb.Dataset(x_test, label = y_test_boost)
 y_lgcox_pred <- y_lgcidx_pred <- matrix(0, n - tt, 5)
for (i in seq_len(5)) {
   y_lgcox_pred[, i] <- predict(lgb_cox_m$boosters[[i]]$booster, x_test)
   y_lgcidx_pred[, i] <- predict(lgb_cix_m$boosters[[i]]$booster, x_test)
 }

 y_lgcox_predict <- -rowMeans(y_lgcox_pred)
 y_lgcidx_predict <- -rowMeans(y_lgcidx_pred)

 validation cindex value
cidx_result <- c( 'LGB_Cox' = concordance(y_test ~ y_lgcox_predict)$con,
                  'LGB_Cidx' = concordance(y_test ~ y_lgcidx_predict)$con)

# importance plot
 imp_lgb=lgb.importance(lgb_cox_m$boosters[[1]]$booster,percentage = TRUE)
 imp_lgb_cix=lgb.importance(lgb_cix_m$boosters[[1]]$booster,percentage = TRUE)
  lgb.plot.importance(imp_lgb,top_n = 10)
  lgb.plot.importance(imp_lgb_cix,top_n = 10)
}
