# Xsurv
This package implements XGBoost and LightGBM in survival data with partiallikelihood function of cox ph model and smoothed C-index as loss functions.
gbm and random forests are also provided in Xsurv function to fit the model.
Additional survival information can be found with XGB and LGB models,including simple survival tree,risk level and SHAP importance. 
## Installation

Please install from Github:
``` r
devtools::install_github("topycyao/Xsurv")
```
## Examples
```{r}
library(Xsurv)
library(survival)
#fit the data 
data(lung)
View(lung)
mydata<-(lung[,-1])

datay_train<-mydata[,c(1,2)]
datax_train<-mydata[,-c(1,2)]

xs<-Xsurv(datax_train,datay_train,top_n = 5)


```
## model analysis


SHAP importance
```{r}
shap=xs$SHAP
shap
```
<p align="center">
  <img src = "https://github.com/topycyao/Xsurv/blob/master/docs%20/figures/shaplung.png" width="500" height="400">
</p>

Suvival tree

```{r}
xm<-xs$mod
xtree<-xs$tree
```
<p align="center">
  <img src = "https://github.com/topycyao/Xsurv/blob/master/docs%20/figures/exampletree.png?raw=true">
</p>

```{r}

###simplified tree created by ctree
#x_ctree<-xtree$tree2
#plot(xctree)

```

Risk analysis
```{r}
library(survminer)
risk=xs$risk
#plot the kaplan-meier curve for different risk levels
fit=risk$fit
ggsurvplot(fit,pval = TRUE,palette = c('coral','burlywood1','cadetblue1'),size=3,
                          legend=c(0.75,0.75),legend.title='',font.x=c(18,"plain","black"),
                           font.y=c(18,"plain","black"))
```
<p align="center">
  <img src = "https://github.com/topycyao/Xsurv/blob/master/docs%20/figures/kmrisk.png?raw=true">
</p>

## Reference

Li,K. et al. Efficient gradient boosting for prognostic biomaker discovery
