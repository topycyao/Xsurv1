\name{surv_risk_aut_lgbcv}
\alias{surv_risk_aut_lgbcv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get risk level for survival data with lgbcv object
}
\description{
\code{surv_risk_m} is a function to get risk level for survival data using lgbcv object
}
\usage{
surv_risk_aut_lgbcv(
model,
x_train,
x_test
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
    model to be applied
}
  \item{x_train}{
    Train data
}
  \item{x_test}{
    Test data
}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
risk level for test data
}

\examples{
library(MASS)
library(survival)
library(dplyr)
library(xgboost)
library(lightgbm)
library(magrittr)

library(survcomp)
library(data.table)
source('loss_func.R')
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

  ### simulation setting
  n <- 1200
  p <- 100
  #rho <- 0.5
  tt <- 1000

  ### auto regressive covariance structure
 V<-diag(p)

  X1 <- mvrnorm(n = 400, mu = rep(0, p), Sigma = V)
  X2 <- mvrnorm(n = 400, mu = rep(10, p), Sigma = V)
  X3 <- mvrnorm(n = 400, mu = rep(15, p), Sigma = V)
  X<-rbind(X1,X2,X3)
  X<-X[sample(1:n),]
  g<-5
  ### nonlinear transformation
  mu <- exp(g*pnorm((X[, 10] > 10) + X[, 20]-1 ) +
              g*pnorm(0.5*X[, 30]+(X[, 40]>5) - 1) +
              g*pnorm( (X[, 50]>10)+(X[, 60])>15) +
              g*pnorm((X[, 70]>20) + X[, 80] ^ 2 - 1) +
              g*pnorm((X[, 90]>20)+X[, 100] ^ 2 - 1))

  ### survival time simulation

  obs_time <- sqrt(-(log(runif(n)))/(mu))
  real_time<-obs_time
  ### True risk level
  l_q<-quantile(mu,0.33)

  h_q<-quantile(mu,0.67)
  kmu=kmeans(mu,3)
  kt=kmeans(real_time,3)
  hist(mu)


  tr_y<-as.data.frame(real_time)

  tr_y$Risk<-ris_tran_tr(mu,l_q,h_q)
  tr_test<-tr_y[(tt + 1) : n,]

  y_ra<-factor(tr_y$Risk,levels = c('High Risk','Medium Risk','Low Risk'))
  y_ra_test<-y_ra[(tt + 1) : n]








  a <- 0.005*rbinom(n = n, size = 1, prob = 1/3)
  b <- runif(n = n, min = 0, max = 0.005)
  a[a == 0] <- b[a == 0]
  C <- a
  status <- as.numeric(obs_time <= C)
  obs_time <- pmin(obs_time, C)

  surv_time <- Surv(obs_time, status)
  surv_time_boost <-  2 * obs_time * (status - .5)
  x_train <- X[seq_len(tt), ]
  y_train <- surv_time[seq_len(tt)]
  y_train_risk<-data.frame(status=status[1:tt],time=obs_time[1:tt])
  y_train_boost <- surv_time_boost[seq_len(tt)]
  x_test <- X[(tt + 1) : n, ]
  y_test <- surv_time[(tt + 1) : n]
  y_test_risk<-data.frame(status=status[1:tt],time=obs_time[1:tt])
  y_test_boost <- surv_time_boost[(tt + 1) : n]




  ### Convert to XGB/LGB dataset
  XDtrain <- xgb.DMatrix(x_train, label = y_train_boost)
  XDtest <- xgb.DMatrix(x_test, label = y_test_boost)

  LDtrain <- lgb.Dataset(x_train, label = y_train_boost)
  LDtest <- lgb.Dataset(x_test, label = y_test_boost)

  d_train <- as.data.frame(x_train)
  d_train %<>% mutate(yy = y_train)
  d_test <- as.data.frame(x_test)

  lgb_cox_m<-lgb.sur(x_train,y_train)


  srxgbcv<-surv_risk_aut_lgbcv(xgb_cox,x_train,x_test)
