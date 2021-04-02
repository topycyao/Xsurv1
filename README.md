# Xsurv
This package implements XGBoost and LightGBM in survival data with partiallikelihood function of cox ph model and smoothed C-index as loss functions.
Additional survival information can be found with XGB and LGB models,including risk level, simple survival tree and etc. 
## Installation

Please install from Github:
``` r
devtools::install_github("topycyao/Xsurv")
```
## Examples
```{r}
library("Xsurv")
library("ggplot2")
library("SHAPforxgboost")
library("xgboost")
library("lightgbm")
library("MASS")
library("survival")
library("dplyr")
library("magrittr")
library("survcomp")
set.seed(981)
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
 mu <- exp(4*pnorm((X[, 10] > 0.5) + X[, 20] ^ 2 - 1) +
             4*pnorm(0.5*X[, 30]+X[, 40]^2 - 1) +
             4*pnorm(0.5 * X[, 50]+X[, 60] ^ 2 - 1) +
             4*pnorm(sin(X[, 70]) + X[, 80] ^ 2 - 1) +
             4*pnorm(cos(X[, 90])+X[, 100] ^ 2 - 1))
# ### survival time simulation

 obs_time <- -(log(runif(n)))/(mu)

 a <- 0.4*rbinom(n = n, size = 1, prob = 1/3)
 b <- runif(n = n, min = 0, max = 0.4)
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
 ###
xgb_cox_m<-xgb.sur(x_train,y_train)
xgb_cix_m<-xgb.sur(x_train,y_train,method = 'C')

XDtest <- xgb.DMatrix(x_test, label = y_test_boost)
y_xgcox_predict <- -rowMeans(sapply(xgb_cox_m$models, predict, XDtest))
y_xgcidx_predict <- -rowMeans(sapply(xgb_cix_m$models, predict, XDtest))

 ###########
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

 #validation cindex value
cidx_result <- c( 'XGB_Cox' = concordance(y_test ~ y_xgcox_predict)$con,
                  'XGB_Cidx' = concordance(y_test ~ y_xgcidx_predict)$con,
                  'LGB_Cox' = concordance(y_test ~ y_lgcox_predict)$con,
                  'LGB_Cidx' = concordance(y_test ~ y_lgcidx_predict)$con)

```
## Importance analysis

SHAP importance plot can be presented.

```{r}
# model selected
x1=xgb_cox_m$models[[1]]
l1= lgb_cox_m$boosters[[5]]$booster
# SHAP importance plot
sh1=shap.plot.summary.wrap1(x1,x_train,top_n = 5)
sh2=shap.plot.summary.wrap1(l1,x_train,top_n = 5)
```
<p align="center">
  <img src = "https://github.com/topycyao/Xsurv/doc/figures/xgb_plot.png"/>
</p>


## Reference

Li,K. et al. Efficient gradient boosting for prognostic biomaker discovery
